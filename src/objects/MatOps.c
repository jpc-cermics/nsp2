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


static void Kronecker (NspMatrix *A,NspMatrix *B,NspMatrix *PK);
typedef int (*AdSu) (int*,double *,int*,double *,int*);
typedef int (*AdSuZ) (int*,doubleC *,int*,doubleC *,int*);
static int MatOpScalar (NspMatrix *Mat1,NspMatrix *Mat2,AdSu F1,AdSuZ F2);

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/*
 * Real A(i,j)=dval, A is changed 
 */

void nsp_mat_set_rval(NspMatrix *A, double dval)
{
  int inc=1;
  switch ( A->rc_type ) 
    { 
    case 'r' :
      nsp_dset(&A->mn,&dval,A->R,&inc);
      break ;
    case 'c' :
      inc=2;
      nsp_dset(&A->mn,&dval,(double *) A->C,&inc);
      break;
    }

}

/*
 * Imag A(i,j)=dval, A is changed and transformed 
 * to complex type if necessary.
 * return 0 on failure 
 */

int nsp_mat_set_ival(NspMatrix *A, double dval)
{
  int inc=1;
  switch ( A->rc_type ) 
    {
    case 'r' :
      if (nsp_mat_complexify(A,dval) == FAIL ) 
	return(FAIL);
      else 
	return(OK);
      break;
    case 'c' :
      nsp_ciset(&A->mn,&dval,A->C,&inc);
      break;
    }
  return(OK);
}


/*
 * A = A op B  
 *   rules : 
 *     A=[] , flag ==1 then A unchanged else A=B and F2(A)
 *     B=[] , flag ==1 then A=[] else A unchanged 
 *     B scalar, F1(A,B) 
 *     A scalar, A=A(1,1)*onesB) et F3(A,B) 
 */

typedef int (*MPM) (NspMatrix *,NspMatrix*);
typedef int (*PM) (NspMatrix*);

static int MatNoOp(NspMatrix *A)
{
  return OK;
}

static int MatOp(NspMatrix *A, NspMatrix *B, MPM F1, PM F2, MPM F3, int flag)
{
  /* A = [] */ 
  if ( A->mn == 0) 
    {
      if ( flag == 1) return OK ; 
      else
	{
	  /* flag != 1 ==> [] op A  returns F4(A) **/
	  /* resize A to B */ 
	  if ( nsp_matrix_fill_with(A,B) == FAIL) return FAIL;
	  return (*F2)(A); 
	}
    }
  /* B = [] */ 
  if ( B->mn == 0) 
    {
      if ( flag == 1) 
	/* flag == 1 ==> A op [] returns [] **/
	return  nsp_matrix_resize(A,0,0); 
      else
	return OK; 
    }
  /* B is scalar */ 
  if ( B->mn == 1) 
    return (*F1)(A,B);
  else if ( A->mn == 1 ) 
    {
      if ( nsp_matrix_scalar_to_mn(A,B->m,B->n) == FAIL) return FAIL; 
    }
  /* general case */
  return (*F3)(A,B); 
}


/* a generic function for Mat op scalar 
 *   Mat1 is a Matrix Mat2 is a scalar 
 *   F1 is used if both argument are real
 *   F2 is used if both are complex 
 **/

static int MatOpScalar(NspMatrix *Mat1, NspMatrix *Mat2, AdSu F1, AdSuZ F2)
{
  int inc=1;
  int incs=0;
  if ( Mat1->rc_type == 'r' ) 
    {
      if ( Mat2->rc_type == 'r') 
	{
	  (*F1)(&(Mat1->mn),Mat2->R,&incs,Mat1->R,&inc);
	}
      else 
	{
	  inc = 1;
	  if (nsp_mat_complexify(Mat1,0.00)== FAIL ) return(FAIL);
	  (*F2)(&(Mat1->mn),Mat2->C,&incs,Mat1->C,&inc);
	}
    }
  else 
    {
      if ( Mat2->rc_type == 'r') 
	{
	  doubleC Z= { Mat2->R[0],0.0};
	  (*F2)(&(Mat1->mn),&Z,&incs,Mat1->C,&inc);
	}
      else 
	{
	  inc = 1;
	  (*F2)(&(Mat1->mn),Mat2->C,&incs,Mat1->C,&inc);
	}
    }
  return(OK);
}


/**
 * nsp_mat_mul:
 * @A: a #NspMatrix 
 * @B: a #NspMatrix
 * 
 * multiplies 2 #NspMatrix @A and @B and returns the product.
 * @A and @B are not modified by this function.
 *
 * Returns a #NspMatrix or %NULLMAT.
 */
 
NspMatrix *nsp_mat_mult(NspMatrix *A, NspMatrix *B)
{  
  doubleC zalpha={1.00,0.00},zbeta={0.00,0.00};
  double alpha=1.00,beta=0.00;
  NspMatrix *Loc;
  int A_is_copied = 0, B_is_copied = 0;

  if ( A->n != B->m ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return NULLMAT;
    }

#ifdef MTLB_MODE
  if ( A->n == 0 )
    {
      if ( (Loc =nsp_matrix_create(NVOID,'r',A->m,B->n)) == NULLMAT ) goto err;
      nsp_mat_set_rval(Loc, 0.0); 
      return Loc;
    }
#endif

  if ( A->rc_type == 'c' ) 
    {
      if ( B->rc_type == 'r' ) 
	{
	  if ( (B =nsp_mat_copy_and_complexify(B)) == NULLMAT ) return NULLMAT; 
	  B_is_copied = 1;
	}
    }
  else 
    { 
      if ( B->rc_type == 'c' ) 
	{
	  if ( (A =nsp_mat_copy_and_complexify(A)) == NULLMAT ) return NULLMAT; 
	  A_is_copied = 1;
	}
    }

  if ( (Loc =nsp_matrix_create(NVOID,A->rc_type,A->m,B->n)) == NULLMAT ) goto err;


  if ( Loc->rc_type == 'c' ) 
    C2F(zgemm)("N","N",&A->m,&B->n,&A->n,&zalpha,A->C,&A->m,B->C,&B->m,
	       &zbeta,Loc->C,&A->m,1,1);
  else 
    C2F(dgemm)("N","N",&A->m,&B->n,&A->n,&alpha,A->R,&A->m,B->R,&B->m,
	       &beta,Loc->R,&A->m,1,1); 

  if ( A_is_copied ) nsp_matrix_destroy(A);
  if ( B_is_copied ) nsp_matrix_destroy(B);
  return Loc;

 err:
  if ( A_is_copied ) nsp_matrix_destroy(A);
  if ( B_is_copied ) nsp_matrix_destroy(B);
  return NULLMAT;
}

/*
 * term to term addition : the general case 
 * A = A+B (covers the scalar and [] cases ) 
 */

int nsp_mat_add(NspMatrix *A, NspMatrix *B) 
{
  return MatOp(A,B,nsp_mat_add_scalar,MatNoOp,nsp_mat_dadd,0);
}

/* deals with the case dim A == dim B **/

int nsp_mat_dadd(NspMatrix *Mat1, NspMatrix *Mat2)
{
  if (SameDim(Mat1,Mat2))
    {
      int inc=1;
      if ( Mat1->rc_type == 'r' ) 
	{
	  if ( Mat2->rc_type == 'r') 
	    {
	      nsp_dadd(&(Mat1->mn),Mat2->R,&inc,Mat1->R,&inc);
	    }
	  else 
	    {
	      double *D1,*D2;
	      inc = 2;
	      if (nsp_mat_complexify(Mat1,0.00)== FAIL ) return(FAIL);
	      D1= (double *) Mat1->C;
	      D2= (double *) Mat2->C;
	      nsp_dadd(&(Mat1->mn),D2,&inc,D1,&inc);
	      C2F(dcopy)(&(Mat1->mn),D2+1,&inc,D1+1,&inc);
	    }
	}
      else 
	{
	  if ( Mat2->rc_type == 'r') 
	    {
	      double *D1;
	      int inc2=2;
	      D1= (double *) Mat1->C;
	      nsp_dadd(&(Mat1->mn),Mat2->R,&inc,D1,&inc2);
	    }
	  else 
	    {
	      inc = 1;
	      nsp_zadd(&(Mat1->mn),Mat2->C,&inc,Mat1->C,&inc);
	    }
	}
      return(OK);
    }

  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
}




/* the case Mat2 scalar **/

int nsp_mat_add_scalar(NspMatrix *Mat1, NspMatrix *Mat2)
{
  return MatOpScalar(Mat1,Mat2,nsp_dadd,nsp_zadd);
}

/* extension of Mat + scalar for (-%inf+ %inf -> -%inf) */

int nsp_mat_add_scalar_maxplus(NspMatrix *Mat1, NspMatrix *Mat2)
{
  return MatOpScalar(Mat1,Mat2,nsp_dadd_maxplus,nsp_zadd_maxplus);
}

int nsp_mat_dadd_maxplus(NspMatrix *Mat1, NspMatrix *Mat2)
{
  if (SameDim(Mat1,Mat2))
    {
      int inc=1;
      if ( Mat1->rc_type == 'r' ) 
	{
	  if ( Mat2->rc_type == 'r') 
	    {
	      nsp_dadd_maxplus(&(Mat1->mn),Mat2->R,&inc,Mat1->R,&inc);
	    }
	  else 
	    {
	      double *D1,*D2;
	      inc = 2;
	      if (nsp_mat_complexify(Mat1,0.00)== FAIL ) return(FAIL);
	      D1= (double *) Mat1->C;
	      D2= (double *) Mat2->C;
	      nsp_dadd_maxplus(&(Mat1->mn),D2,&inc,D1,&inc);
	      C2F(dcopy)(&(Mat1->mn),D2+1,&inc,D1+1,&inc);
	    }
	}
      else 
	{
	  if ( Mat2->rc_type == 'r') 
	    {
	      double *D1;
	      int inc2=2;
	      D1= (double *) Mat1->C;
	      nsp_dadd_maxplus(&(Mat1->mn),Mat2->R,&inc,D1,&inc2);
	    }
	  else 
	    {
	      inc = 1;
	      nsp_zadd_maxplus(&(Mat1->mn),Mat2->C,&inc,Mat1->C,&inc);
	    }
	}
      return(OK);
    }

  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
}


/*
 * term to term addition : the general case 
 * A = A-B (covers the scalar and [] cases ) 
 */

int nsp_mat_sub(NspMatrix *A, NspMatrix *B) 
{
  return MatOp(A,B,nsp_mat_sub_scalar,nsp_mat_minus,nsp_mat_dsub,0);
}

/* the case dim A == dim B **/

int nsp_mat_dsub(NspMatrix *Mat1, NspMatrix *Mat2)
{
  if (SameDim(Mat1,Mat2))
    {
      int inc=1;
      if ( Mat1->rc_type == 'r' ) 
	{
	  if ( Mat2->rc_type == 'r') 
	    {
/* 	      double alpha=-1.0; */
/* 	      C2F(daxpy)(&(Mat1->mn),&alpha,Mat2->R,&inc,Mat1->R,&inc); */
	      nsp_dsub(&(Mat1->mn),Mat2->R,&inc,Mat1->R,&inc);
	    }
	  else 
	    {
	      int i;
	      double *D1,*D2;
	      inc = 2;
	      if (nsp_mat_complexify(Mat1,0.00)== FAIL ) return(FAIL);
	      D1= (double *) Mat1->C;
	      D2= (double *) Mat2->C;
	      nsp_dsub(&(Mat1->mn),D2,&inc,D1,&inc);
	      for ( i = 0 ; i < Mat1->mn ; i++) Mat1->C[i].i = - Mat2->C[i].i;
	    }
	}
      else 
	{
	  if ( Mat2->rc_type == 'r') 
	    {
	      double *D1;
	      int inc2=2;
	      D1= (double *) Mat1->C;
	      nsp_dsub(&(Mat1->mn),Mat2->R,&inc,D1,&inc2);
	    }
	  else 
	    {
	      inc = 1;
	      nsp_zsub(&(Mat1->mn),Mat2->C,&inc,Mat1->C,&inc);
	    }
	}
      return(OK);
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
}

/* the case Mat2  is scalar  (No check is made) */

int nsp_mat_sub_scalar(NspMatrix *Mat1, NspMatrix *Mat2)
{
  return MatOpScalar(Mat1,Mat2,nsp_dsub,nsp_zsub);
}

/* the case Mat2  is scalar  (No check is made) */

int nsp_mat_sub_scalar_maxplus(NspMatrix *Mat1, NspMatrix *Mat2)
{
  return MatOpScalar(Mat1,Mat2,nsp_dsub_maxplus,nsp_zsub_maxplus);
}

/* the case  Mat1 = - Mat1 + Mat2 
 * Mat2 is assumed to be a scalar ( No check is made) */ 

int nsp_mat_subs_calarm(NspMatrix *Mat1, NspMatrix *Mat2)
{
  if ( MatOpScalar(Mat1,Mat2,nsp_dsub,nsp_zsub) == FAIL ) 
    return FAIL;
  nsp_mat_minus(Mat1);
  return OK;
}

/*
 * A = Matclean(a) clean A according to epsa and epsr 
 * epsa is used if rhs >= 1 
 * epsr is used if rhs >= 2
 * A is changed, 
 */

void nsp_mat_clean(NspMatrix *A, int rhs, double epsa, double epsr)
{
  int j;
  double d_epsr=DBL_EPSILON;
  double d_epsa=DBL_EPSILON;
  double norm,eps;
  int inc=1;
  if ( A->rc_type == 'r') 
    norm=C2F(dasum)(&A->mn,A->R,&inc);
  else
    norm=nsp_zasum(&A->mn,A->C,&inc);
  if ( rhs >= 2 ) d_epsa = epsa;
  if ( rhs >= 3 ) d_epsr = epsr;
  eps= Max(epsa,epsr*norm);
  switch ( A->rc_type ) 
    {
    case 'r' : 
      for ( j = 0 ; j < A->mn  ; j++ ) if ( Abs(A->R[j]) < eps) A->R[j] = 0.0; 
      break;
    case 'c': 
      for ( j = 0 ; j < A->mn  ; j++ ) 
	{
	  if ( Abs(A->C[j].r) < eps) A->C[j].r = 0.0;
	  if ( Abs(A->C[j].i) < eps) A->C[j].i = 0.0;
	}
    }
}


/*
 *  Utility function for computing the max of a list of matrices 
 *
 *  A(k,l) = Maxi(A(k,l),B(k,l)) 
 *  Ind(k,l) is set to j if B(k,l) realize the max and flag ==1 
 *  if flag == 0 Ind is unused and can be null
 *  B unchanged A changed, A is enlarged if necessary Ind also 
 *  A and B must have same size or be scalars. 
 *  On entry A and Ind if Ind is used must have the same size 
 */

int nsp_mat_maxitt1(NspMatrix *A, NspMatrix *B, NspMatrix *Ind,int j,int flag)
{
  int i;
  if (  SameDim(A,B) )
    {
      for ( i = 0; i < A->mn ; i++ ) 
	if  ( ISNAN(A->R[i])  ||  A->R[i] <  B->R[i] ) 
	  {
	    A->R[i] = B->R[i] ;
	    if ( flag == 1  &&  !ISNAN(B->R[i]) ) Ind->R[i] = j;
	  }
    }
  else if ( B->mn == 1) 
    {
      for ( i = 0; i < A->mn ; i++ ) 
	if  ( ISNAN(A->R[i])  ||  A->R[i] <  B->R[0] ) 
	  {
	    A->R[i] = B->R[0] ;
	    if (flag == 1  &&  !ISNAN(B->R[0]) ) Ind->R[i] = j;
	  }
    }
  else if ( A->mn == 1) 
    {
      /* A is scalar and B is not: A must be enlarged **/
      /* and Ind also if flag == 1**/
      double aval = A->R[0];
      if ( nsp_matrix_resize(A,B->m,B->n) == FAIL) return FAIL;
      if ( flag == 1)
	{
	  int  indval = (int) Ind->R[0];
	  if ( nsp_matrix_resize(Ind,B->m,B->n) == FAIL) return FAIL;
	  nsp_mat_set_rval(Ind,indval);
	}
      for ( i = 0; i < A->mn ; i++ ) 
	if  ( ISNAN(aval)  ||  aval < B->R[i] ) 
	  {
	    A->R[i] = B->R[i] ;
	    if ( flag == 1  &&  !ISNAN(B->R[i]) ) Ind->R[i] = j;
	  }
	else
	  A->R[i] = aval;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
  return(OK);
}


/*
 *  A(k,l) = Mini(A(k,l),B(k,l)) 
 *  Ind(k,l) is set to j if B(k,l) realize the max and flag ==1 
 *  if flag == 0 Ind is unused and can be null
 *  B unchanged A changed, A is enlarged if necessary 
 *  A and B must have same size or be scalars.
 */

int nsp_mat_minitt1(NspMatrix *A, NspMatrix *B, NspMatrix *Ind, int j, int flag)
{
  int i;
  if (  SameDim(A,B) )
    {
      for ( i = 0; i < A->mn ; i++ ) 
	if  ( ISNAN(A->R[i]) ||  A->R[i] >  B->R[i] ) 
	  {
	    A->R[i] = B->R[i] ;
	    if ( flag == 1  &&  !ISNAN(B->R[i]) ) Ind->R[i] = j;
	  }
    }
  else if ( B->mn == 1) 
    {
      for ( i = 0; i < A->mn ; i++ ) 
	if  ( ISNAN(A->R[i])  || A->R[i] >  B->R[0] ) 
	  {
	    A->R[i] = B->R[0] ;
	    if ( flag == 1  &&  !ISNAN(B->R[0]) ) Ind->R[i] = j;
	  }
    }
  else if ( A->mn == 1) 
    {
      /* A is scalar and B is not: A must be enlarged **/
      double aval = A->R[0];
      if ( nsp_matrix_resize(A,B->m,B->n) == FAIL) return FAIL;
      for ( i = 0; i < A->mn ; i++ ) 
	if  ( ISNAN(aval) ||  aval > B->R[i] ) 
	  {
	    A->R[i] = B->R[i] ;
	    if ( flag == 1  &&  !ISNAN(B->R[i]) ) Ind->R[i] = j;
	  }
	else
	  A->R[i] = aval;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
  return(OK);
}


/*
 *  Read a Set of Matrix(p,q) in the file file 
 * The file is written as follows 
 *
 * BeginMat Name type 
 * m(1,1) m(1,2) .... m(1,q)
 * m(2,1) m(2,2) .... ...
 * .... 
 * m(p,1) .....  .... m(P,q)
 * EndMat 
 * 
 * Maximu number of matrices : 100 
 */

static char Info[256];

NspMatrix **nsp_mat_slec(char *file, int *Count)
{ 
  int cols[100],lines[100];
  int MatCount,MatCount1,i,j;
  NspMatrix **Loc;
  FILE   *fopen(const char *, const char *), *fd;
  /* first reading to get the matrix sizes **/
  fd=fopen(file,"r");
  if ( fd == 0) 
    {
      Scierror("Error:\tOpen Error : file %s Not found\n",file);
      return(( NspMatrix **) 0) ;
    }
  MatCount1=0;
  while(1)
    {
      char matname[10],typ[2];
      int ligne=0,n=1,num=0;
      strcpy(Info,"--------");
      while ( strncmp("BeginMat",Info,8) !=0 && n != EOF )
	n=nsp_mat_readline(fd);
      if ( n == EOF ) break;
      num=sscanf(Info,"%*s%10s%1s",matname,typ);
      if ( num != 2 ) 
	{
	  fprintf(stderr,"Missing items in Mat declaration : <%s> ?\n",Info);
	  typ[0]='r';
	  strcpy(matname,"Noname");
	}
      n=nsp_mat_readline(fd);
      cols[MatCount1++]=nsp_numtokens(Info);
      while (n != EOF &&  n != 0 && strncmp(Info,"EndMat",6) != 0)
	{ 
	  n=nsp_mat_readline(fd);
	  ligne++;
	}
      if ( n != EOF && n != 0) 
	lines[MatCount1-1 ]=ligne;
      if ( n == EOF ) 
	{
	  fprintf(stderr,"Missing End of Matrix <%s>definition \n",matname);
	  MatCount1--; break;}
      if ( n == 0 ) 
	{
	  fprintf(stderr,"Wrong Matrix <%s> End definition\n",matname);
	  MatCount1--; break;}
      if ( n == EOF ) break;
    }
  Loc = ( NspMatrix **) MALLOC(MatCount1 *sizeof( NspMatrix *));
  /* second pass to read **/
  fclose(fd);
  fd=fopen(file,"r");
  if ( fd == 0) 
    {
      Scierror("Error:\tCannot open file %s\n",file);
      return(( NspMatrix **) 0);
    }
  MatCount=0;
  while(1)
    {
      int num=0,n=1;
      char matname[11],typ[2];
      strcpy(Info,"--------");
      while ( strncmp("BeginMat",Info,8) !=0 && n != EOF )
	n=nsp_mat_readline(fd); 
      if ( n == EOF ) break;
      if ( ++MatCount > MatCount1 ) break;
      num=sscanf(Info,"%*s%10s%1s",matname,typ);
      if ( num != 2 ) 
	fprintf(stderr,"Missing items in Mat declaration : <%s> ?\n",Info);
      if ( typ[0] != 'r' || typ[0] != 'c' ) typ[0]='r';
      Loc[MatCount-1]= nsp_matrix_create(matname,typ[0],lines[MatCount-1],
					 cols[MatCount-1]);
      /* XXXX Pb a regler **/
      if ( Loc[MatCount-1] == NULLMAT ) return Loc;
      for (i=0;i < Loc[MatCount-1]->m;i++)
	for (j=0;j < Loc[MatCount-1]->n;j++)
	  { 
	    double xloc;
	    fscanf(fd,"%lf",&xloc);
	    Loc[MatCount-1]->R[i+Loc[MatCount-1]->m*j]=xloc;
	  }
    }
  *Count = MatCount1;
  return(Loc);
}


/*
 * Read one matrix in a file 
 * The file is written as follows 
 */

NspMatrix *MatLec(FILE *fd)
{ 
  NspMatrix *Loc;
  int i,j,rows=0,cols=0,vl=0,n=0;
  while(1)
    {
      strcpy(Info,"--------");
      while ( sscanf(Info,"%*f") <= 0 ) { n=nsp_mat_readline(fd); vl++;}
      if ( n == EOF ) break;
      cols =nsp_numtokens(Info);
      while (n != EOF &&  n != 0 ) 
	{ 
	  n=nsp_mat_readline(fd);
	  rows++;
	}
    }
  if ((Loc = nsp_matrix_create(NVOID,'r',rows,cols)) == NULLMAT ) return Loc;
  /* second pass to read **/
  rewind(fd);
  for ( i = 0 ; i < vl ; i++)nsp_mat_readline(fd);
  for (i=0; i < rows ;i++)
    for (j=0;j < cols;j++)
      { 
	double xloc;
	fscanf(fd,"%lf",&xloc);
	Loc->R[i+Loc->m*j]=xloc;
      }
  return(Loc);
}

/*

NspMatrix* fooBOU(void)
{
FILE *fd ;
fd = fopen("poo","r");
return MatLec(fd);
fclose(fd);
}
*/


/*
 *nsp_mat_readline: reads one line in file fd and puts the result in Info
 */

int nsp_mat_readline(FILE *fd)
{
  int n;
  n= fscanf(fd,"%[^\n]%*c",Info);
  if ( n==0) n=fscanf(fd,"%*c");
  return(n);
}

/*
 * Test de TestNumTokens 
 */

void nsp_testnumtokens(void)
{
  char buf[30], format[20];
  strcpy(format,"%d Tokens in <%s>\n");
  strcpy(buf,"un deux trois");fprintf(stderr,format,nsp_numtokens(buf),buf);
  strcpy(buf,"un");  fprintf(stderr,format,nsp_numtokens(buf),buf);
  strcpy(buf,"un deux trois  "); fprintf(stderr,format,nsp_numtokens(buf),buf);
  strcpy(buf,"un\tdeux\ttrois\n"); fprintf(stderr,format,nsp_numtokens(buf),buf);
  fprintf(stderr,format,nsp_numtokens((char *) 0) , ((char *) 0));
  strcpy(buf,"un\t");  fprintf(stderr,format,nsp_numtokens(buf),buf);
  strcpy(buf," \t\nun");  fprintf(stderr,format,nsp_numtokens(buf),buf);
  strcpy(buf,"1.0  1.0");fprintf(stderr,format,nsp_numtokens(buf),buf);
}

int nsp_numtokens(char *string)
{
  char buf[128];
  int n=0,lnchar=0,ntok=-1;
  if (string != 0)
    { 
      /* Counting leading white spaces **/
      sscanf(string,"%*[ \t\n]%n",&lnchar);
      while ( n != EOF && lnchar <= (int) strlen(string)+1 )
	{ 
	  int nchar1=0,nchar2=0;
	  ntok++;
	  n= sscanf(&(string[lnchar]),
		    "%[^ \n\t]%n%*[ \t\n]%n",buf,&nchar1,&nchar2);
	  lnchar += (nchar2 <= nchar1) ? nchar1 : nchar2 ;
	}
      return(ntok);
    }
  return(FAIL);
}


/*
 * Change a matrix of Real type to Imaginary type 
 * return 0 on allocation failure 
 * The imag part is initialized with the value d
 */

void nsp_ciset(const int *n,const double *z, doubleC *tab, const int *inc)
{
  int i;
  if ( *inc > 0 ) 
    for (i=0; i < *n ; i += *inc) tab[i].i = *z;
  else 
    for (i=*n-1; i >= 0 ; i += *inc) tab[i].i = *z;
}

void nsp_csetd(const int *n,const double *z,doubleC *tab,const int *inc) 
{
  int i;
  if ( *inc > 0 )
    for (i=0; i < *n ; i += *inc) 
      {
	tab[i].i = 0.00 ;tab[i].r = *z;
      }
  else 
    for (i=*n-1; i >= 0 ; i += *inc) 
      {
	tab[i].i = 0.00 ;tab[i].r = *z;
      }
    
}

/*
 * Turns Mat into a Complex Matrix with imag part set to 
 * 0. Mat is changed 
 */

int nsp_mat_complexify(NspMatrix *Mat, double d)
{
  double *R;
  /* int incx=1,incy=2 */
  int i;
  if ( Mat->rc_type == 'c' ) return(OK);
  /* take care that R and C are at the same memory location */
  R = Mat->R;
  Mat->C =nsp_alloc_doubleC(Mat->mn);
  if ( Mat->C == (doubleC *) 0) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return FAIL;
    }
  Mat->rc_type = 'c';
  /*   nsp_ciset(&(Mat->mn),&d,Mat->C,&incx);   */
  /*   C2F(dcopy)(&(Mat->mn),R,&incx,(double *) Mat->C,&incy); */
  for ( i = 0 ; i < Mat->mn ; i++ )
    { Mat->C[i].r = R[i]; Mat->C[i].i = d; }
  FREE(R);
  return OK;
}

/**
 * nsp_mat_copy_and_complexify:
 * @A: a #NspMatrix 
 *
 * copies a real #NspMatrix @A and returns the complexified copy (with
 * zeros as imaginary part) or %NULLMAT.
 *
 * %NULLMAT is returned in case of malloc problem or if @A is a complex matrix 
 *
 * Returns a #NspMatrix or %NULLMAT.
 */

NspMatrix *nsp_mat_copy_and_complexify(const NspMatrix *A)
{
  NspMatrix *Mat;
  int i;

  if ( A->rc_type == 'c' )
    return NULLMAT;

  if ( (Mat =nsp_matrix_create(NVOID,'c',A->m,A->n)) == NULLMAT) return NULLMAT;

  for ( i = 0 ; i < A->mn ; i++ )
    {
      Mat->C[i].r = A->R[i];
      Mat->C[i].i = 0.0;
    }
  Mat->convert = A->convert; 
  return Mat;
}

/*
 * Return the Real part of Matrix A 
 * In a Real Matrix A
 * A is changed if a is not real 
 * A = real(A) 
 */

int nsp_mat_get_real(NspMatrix *A)
{
  doubleC *C;
  int incx=2,incy=1;
  if ( A->rc_type == 'r' )  return(OK);
  /* take care that R and C are at the same memory location */
  C = A->C;
  A->R =nsp_alloc_doubles(A->mn);
  if ( A->R == (double *) 0) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(FAIL);
    }
  C2F(dcopy)(&(A->mn),(double *) C,&incx,A->R,&incy);
  FREE(C) ;
  A->rc_type = 'r';
  return(OK);
}

/*
 * Return the Imaginary part of Matrix A 
 * In a Real Matrix A. A= Imag(A) 
 * A is changed  
 */

int nsp_mat_get_imag(NspMatrix *A)
{
  double d=0.0;
  int inc=1,incy=2;
  if ( A->rc_type == 'r')
    {
      nsp_dset(&A->mn,&d,A->R,&inc);
    }
  else
    {
      /* take care that R and C are at the same memory location */
      doubleC *C = A->C;
      A->R =nsp_alloc_doubles(A->mn);
      if ( A->R == (double *) 0) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return(FAIL);
	}
      C2F(dcopy)(&(A->mn),((double *) C)+1,&incy,A->R,&inc);
      A->rc_type = 'r';
      FREE(C) ;
    }
  return OK;
}

/*
 * nsp_mat_inv_el: a(i,j)=1/a(i,j) A est changee
 */

int nsp_mat_inv_el(NspMatrix *A)
{
  int k;
  if( A->rc_type == 'r' )
    {
      for ( k=0 ; k < A->mn ; k++ )
	{
	  A->R[k] = 1.0/A->R[k];
	}
    }
  else
    {
      for (  k=0 ; k < A->mn ; k++ )
	{
	  double sr,si,s;
	  sr=A->C[k].r; si=A->C[k].i; s=sr*sr+si*si;
	  A->C[k].r=sr/s; A->C[k].i=-si/s;
	}
    }
  return(OK);
}

/*
 *nsp_mat_kron: produit de Kroeneker
 * A et B sont inchanges 
 */

NspMatrix *nsp_mat_kron(NspMatrix *A, NspMatrix *B)
{
  char type ;
  NspMatrix *Loc;
  type = ( A->rc_type == 'c' || B->rc_type == 'c') ? 'c' : 'r' ;
  if ((Loc = nsp_matrix_create(NVOID,type,B->m*A->m,B->n*A->n)) == NULLMAT) return(NULLMAT);
  if ( Loc->mn == 0 ) return(Loc);
  Kronecker(A,B,Loc);
  return(Loc);
}

/*
 * nsp_mat_sort: Index=Sort(A)
 * A is changed, Index created with the indexes 
 * return NULLMAT on error 
 * WARNING : A must be real but the test is not done here 
 * ======
 * to be updated XXX
 */

NspMatrix *nsp_mat_sort(NspMatrix *A, int flag, char *str1, char *str2)
{
  int iflag=0,inc=-1,*iloc=NULL,Locm=A->m,Locn=A->n;
  NspMatrix *Loc=NULL;
  if ( flag == 2 )
    {
      if ( str1[0] == 'l' ) 
	{
	  if ( str1[1] == 'r' ) Locn=Min(Locn,1);
	  else Locm=Min(Locm,1);
	}
      if ((Loc= nsp_matrix_create(NVOID,'r',Locm,Locn))  == NULLMAT) return(NULLMAT);
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

/*
 * Sum =nsp_mat_sum(A ,B])
 *     A is unchanged 
 * if B= 'c'|'C' the sum for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r'|'R' the sum for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f'|'F' the full sum is computed 
 * else 'f' is assumed 
 */

NspMatrix *nsp_mat_sum(NspMatrix *A, char *flag)
{
  NspMatrix *Sum;
  int j;
  int inc=1;
  if ( A->mn == 0) 
    {
      if ( flag[0] == 'F' || flag[0]=='f' )
	{
	  Sum = nsp_matrix_create(NVOID,'r',1,1);
	  if ( Sum != NULLMAT) Sum->R[0]=0;
	  return Sum;
	}
      else 
	return  nsp_matrix_create(NVOID,'r',0,0);
    }
  switch (flag[0]) 
    {
    default : 
      Sciprintf("\nInvalid flag '%c' assuming flag='f'\n",flag[0]);
    case 'f': 
    case 'F':
      if ((Sum = nsp_matrix_create(NVOID,A->rc_type,1,1)) == NULLMAT) 
	return(NULLMAT);
      switch ( A->rc_type) 
	{
	case 'r' : 
	  Sum->R[0] =nsp_dsum(&A->mn,A->R,&inc); ;break ;
	case 'c' :  
	  nsp_zsum(&Sum->C[0],&A->mn,A->C,&inc); break;
	}
      break;
    case 'r':
    case 'R':
      if ((Sum = nsp_matrix_create(NVOID,A->rc_type,1,A->n)) == NULLMAT) 
	return NULLMAT;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( j= 0 ; j < A->n ; j++) 
	    Sum->R[j] =nsp_dsum(&A->m,A->R+(A->m)*j,&inc); 
	  break ;
	case 'c' :  
	  for ( j= 0 ; j < A->n ; j++) 
	    nsp_zsum(&Sum->C[j],&A->m,A->C+(A->m)*j,&inc); break;
	  break;
	}
      break;
    case 'c':
    case 'C':
      if ((Sum = nsp_matrix_create(NVOID,A->rc_type,A->m,1)) == NULLMAT) 
	return NULLMAT;
      inc = A->m;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( j= 0 ; j < A->m ; j++) 
	    Sum->R[j] =nsp_dsum(&A->n,A->R+j,&inc); 
	  break ;
	case 'c' :  
	  for ( j= 0 ; j < A->m ; j++) 
	    nsp_zsum(&Sum->C[j],&A->n,A->C+j,&inc); break;
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

NspMatrix *nsp_mat_prod(NspMatrix *A, char *flag)
{
  NspMatrix *Prod;
  int j;
  int inc=1,zero=0;
  if ( A->mn == 0) 
    {
      if ( flag[0] == 'F' || flag[0]=='f' )
	{
	  Prod = nsp_matrix_create(NVOID,'r',1,1);
	  if ( Prod != NULLMAT) Prod->R[0]= 1.0;
	  return Prod;
	}
      else 
	return  nsp_matrix_create(NVOID,'r',0,0);
    }
  switch (flag[0]) 
    {
    default : 
      Sciprintf("\nInvalid flag '%c' assuming flag='f'\n",flag[0]);
    case 'f': 
    case 'F':
  
      if ((Prod = nsp_matrix_create(NVOID,A->rc_type,1,1)) == NULLMAT) 
	return(NULLMAT);
      switch ( A->rc_type) 
	{
	case 'r' : 
	  Prod->R[0] =1.00;
	  nsp_dvmul(&A->mn,A->R,&inc,Prod->R,&zero); ;break ;
	case 'c' :  
	  Prod->C[0].r =1.00 ; Prod->C[0].i = 0.00;
	  nsp_zvmul(&A->mn,A->C,&inc,Prod->C,&zero); break;
	}
      break;
    case 'r':
    case 'R':
      if ((Prod = nsp_matrix_create(NVOID,A->rc_type,1,A->n)) == NULLMAT) 
	return NULLMAT;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      Prod->R[j] =1.00;
	      nsp_dvmul(&A->m,A->R+(A->m)*j,&inc,Prod->R+j,&zero); 
	    }
	  break ;
	case 'c' :  
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      Prod->C[j].r =1.00 ; Prod->C[j].i = 0.00;
	      nsp_zvmul(&A->m,A->C+(A->m)*j,&inc,Prod->C+j,&zero);
	    }
	  break;
	}
      break;
    case 'c':
    case 'C':
  
      if ((Prod = nsp_matrix_create(NVOID,A->rc_type,A->m,1)) == NULLMAT) 
	return NULLMAT;
      inc = A->m;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( j= 0 ; j < A->m ; j++) 
	    {
	      Prod->R[j] =1.00;
	      nsp_dvmul(&A->n,A->R+j,&inc,Prod->R+j,&zero); 
	    }
	  break ;
	case 'c' :  
	  for ( j= 0 ; j < A->m ; j++) 
	    {
	      Prod->C[j].r =1.00 ; Prod->C[j].i = 0.00;
	      nsp_zvmul(&A->n,A->C+j,&inc,Prod->C+j,&zero);
	    }
	  break;
	}
      break;
    }
  return Prod;
}


/*
 *nsp_mat_cum_prod: Cumulative Product of all elements of A
 * A is unchanged 
 */

NspMatrix *nsp_mat_cum_prod(NspMatrix *A, char *flag)
{
  double cuprod;
  doubleC C_cuprod;
  NspMatrix *Prod;
  int i,j;
  if ( A->mn == 0) return nsp_matrix_create(NVOID,'r',0,0);
  if ((Prod = nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT) 
    return NULLMAT;
  switch (flag[0]) 
    {
    default : 
      Sciprintf("\nInvalid flag '%c' assuming flag='f'\n",flag[0]);
    case 'f': 
    case 'F':
      switch ( A->rc_type) 
	{
	case 'r' : 
	  cuprod=1.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    Prod->R[i] = (cuprod *= A->R[i]);
	  break;
	case 'c' :  
	  C_cuprod.r  = 1.00 ; C_cuprod.i = 0.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    { 
	      nsp_prod_c(&C_cuprod,&A->C[i]);
	      Prod->C[i].r = C_cuprod.r;
	      Prod->C[i].i = C_cuprod.i;
	    }
	}
      break;
    case 'r':
    case 'R':
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      cuprod=1.00;
	      for ( i=0 ; i < A->m ; i++) 
		Prod->R[i+(A->m)*j] = (cuprod *= A->R[i+(A->m)*j]);
	    }
	  break ;
	case 'c' :  
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      C_cuprod.r  = 1.00 ; C_cuprod.i = 0.00;
	      for ( i=0 ; i < A->m ; i++) 
		{ 
		  nsp_prod_c(&C_cuprod,&A->C[i+j*A->m]);
		  Prod->C[i+j*A->m].r = C_cuprod.r;
		  Prod->C[i+j*A->m].i = C_cuprod.i;
		}
	    }
	  break;
	}
      break;
    case 'c':
    case 'C':
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( j= 0 ; j < A->m ; j++) 
	    {
	      cuprod=1.00;
	      for ( i=0 ; i < A->n ; i++) 
		Prod->R[j+(A->m)*i] = (cuprod *= A->R[j+(A->m)*i]);
	    }
	  break ;
	case 'c' :  
	  for ( j= 0 ; j < A->m ; j++) 
	    {
	      C_cuprod.r  = 1.00 ; C_cuprod.i = 0.00;
	      for ( i=0 ; i < A->n ; i++) 
		{ 
		  nsp_prod_c(&C_cuprod,&A->C[j+i*A->m]);
		  Prod->C[j+i*A->m].r = C_cuprod.r;
		  Prod->C[j+i*A->m].i = C_cuprod.i;
		}
	    }
	  break;
	}
      break;
    }
  return Prod;
}

/*
 *nsp_mat_cum_sum: Cumulative Sum of all elements of A
 * A is unchanged 
 */

NspMatrix *nsp_mat_cum_sum(NspMatrix *A, char *flag)
{
  double cusum;
  doubleC C_cusum;
  NspMatrix *Sum;
  int i,j;

  if ( A->mn == 0) return  nsp_matrix_create(NVOID,'r',0,0);

  if ((Sum = nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT) 
    return NULLMAT;
  switch (flag[0]) 
    {
    default : 
      Sciprintf("\nInvalid flag '%c' assuming flag='f'\n",flag[0]);
    case 'f': 
    case 'F':
      switch ( A->rc_type) 
	{
	case 'r' : 
	  cusum=0.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    Sum->R[i] = (cusum += A->R[i]);
	  break;
	case 'c' :  
	  C_cusum.r  = 0.00 ; C_cusum.i = 0.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    { 
	      Sum->C[i].r = ( C_cusum.r += A->C[i].r);
	      Sum->C[i].i = ( C_cusum.i += A->C[i].i);
	    }
	}
      break;
    case 'r':
    case 'R':
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      cusum=0.00;
	      for ( i=0 ; i < A->m ; i++) 
		Sum->R[i+(A->m)*j] = (cusum += A->R[i+(A->m)*j]);
	    }
	  break ;
	case 'c' :  
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
	}
      break;
    case 'c':
    case 'C':
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( j= 0 ; j < A->m ; j++) 
	    {
	      cusum=0.00;
	      for ( i=0 ; i < A->n ; i++) 
		Sum->R[j+(A->m)*i] = (cusum += A->R[j+(A->m)*i]);
	    }
	  break ;
	case 'c' :  
	  for ( j= 0 ; j < A->m ; j++) 
	    {
	      C_cusum.r  = 0.00 ; C_cusum.i = 0.00;
	      for ( i=0 ; i < A->n ; i++) 
		{ 
		  Sum->C[j+i*A->m].r = ( C_cusum.r +=A->C[j+i*A->m].r);
		  Sum->C[j+i*A->m].i = ( C_cusum.i +=A->C[j+i*A->m].i);
		}
	    }
	  break;
	}
      break;
    }
  return Sum;
}


/*
 * Max =nsp_mat_maxi(A,B,Imax,lhs)
 *     A is unchanged 
 * if B= 'c' the max for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the max for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the maximum 
 * Imax is created if lhs == 2 
 */

typedef int (*MaMi) (int,double *,int,double *);

static NspMatrix *MatMaxiMini(NspMatrix *A, char *flag, NspMatrix **Imax, int lhs, MaMi F)
{
  NspMatrix *M;
  int j;
  int inc=1,imax;
  if ( A->mn == 0 )    
    {
      if ( lhs == 2) *Imax = nsp_matrix_create(NVOID,'r',0,0);
      return nsp_matrix_create(NVOID,'r',0,0);
    }
  switch (flag[0]) 
    {
    default :
      Sciprintf("\nInvalid flag '%c' assuming flag='f'\n",flag[0]);
    case 'f': 
    case 'F':
      if ((M = nsp_matrix_create(NVOID,A->rc_type,1,1)) == NULLMAT) 
	return(NULLMAT);
      imax = (*F)(A->mn,A->R,1,&M->R[0]);
      if ( lhs == 2 ) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT)
	    return NULLMAT; 
	  (*Imax)->R[0] = imax;
	}
      break;
    case 'r':
    case 'R':
      if ((M = nsp_matrix_create(NVOID,A->rc_type,1,A->n)) == NULLMAT) 
	return NULLMAT;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',1,A->n)) == NULLMAT) 
	    return NULLMAT; 
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      (*Imax)->R[j]=(*F)(A->m,A->R+(A->m)*j,1,&M->R[j]); 
	    }
	}
      else 
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    (*F)(A->m,A->R+(A->m)*j,1,&M->R[j]); 
	  }
      break ;
    case 'c':
    case 'C':
      if ((M = nsp_matrix_create(NVOID,A->rc_type,A->m,1)) == NULLMAT) 
	return NULLMAT;
      inc = A->m;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',A->m,1)) == NULLMAT) 
	    return NULLMAT; 
	  for ( j= 0 ; j < A->m ; j++) 
	    (*Imax)->R[j] = (*F)(A->mn,A->R+j,inc,&M->R[j]);
	}
      else
	for ( j= 0 ; j < A->m ; j++) (*F)(A->mn,A->R+j,inc,&M->R[j]);
      break;
    }
  return M;
}

static int VMaxi(int n, double *A, int incr, double *amax)
{
  int imax,i,i1=1;
  imax = 1;
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


NspMatrix *nsp_mat_maxi(NspMatrix *A, char *flag, NspMatrix **Imax, int lhs)
{
  return MatMaxiMini(A,flag,Imax,lhs,VMaxi);
}



/*
 *nsp_mat_mini: Mini(A)
 * A is unchanged 
 * rs and ri are set to the result 
 */


static int VMini(int n, double *A, int incr, double *amin)
{
  int imin,i,i1=0;
  *amin= A[0]; imin = 1;

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



NspMatrix *nsp_mat_mini(NspMatrix *A, char *flag, NspMatrix **Imax, int lhs)
{
  return MatMaxiMini(A,flag,Imax,lhs,VMini);
}


/* compute at the same time the min and max of a vector 
 * (routine introduced by bruno). Note that it is difficult
 * to use the minmax algorithm (which compares first A[i] and A[i+1]
 * before comparing one to the current minimum and the other to the
 * current maximum) because of eventual Nan components.
 */
static void VMiniMaxi(int n, double *A, int incr, double *amin, double *amax,
		      int *imin, int *imax)
{
  int i,i1;
  *imin = *imax = 1;
  *amax = *amin = A[0];

  /* look for the first non Nan component */
  i = 0; i1 = 1;
  while ( i < n && ISNAN(A[i]) ) { i += incr; i1++; }

  if ( i < n )  /* this vector is not only with Nan... */
    {
      /* init with the first non Nan component then do the usual loop */
      *amin = *amax = A[i]; *imin = *imax = i1;
      for (  ; i < n ; i += incr, i1++ )
	{
	  if ( A[i] < *amin )
	    {
	      *amin = A[i]; *imin = i1;
	    }
	  else if ( A[i] > *amax )
	    {
	      *amax = A[i];
	      *imax = i1;
	    }
	}
    }
  return;
}


/*
 * int nsp_mat_minmax: 
 * A is unchanged 
 * Amin, Amx, Imin, Imax are set to the result
 * routine introduced by bruno 
 */
int nsp_mat_minmax(NspMatrix *A, char *str, NspMatrix **Amin, NspMatrix **Imin,
		   NspMatrix **Amax, NspMatrix **Imax, int lhs)
{
  NspMatrix *amin=NULLMAT, *imin=NULLMAT, *amax=NULLMAT, *imax=NULLMAT;
  int k, m, n, flag, indmin, indmax;

  switch (str[0]) 
    {
    default:
      Sciprintf("\nInvalid flag '%c'  must be 'r','c' or 'F'\n",str[0]);
      return FAIL;
    case 'f': 
    case 'F':
      m = 1; n = 1; flag = 0; break;
    case 'r':
    case 'R':
      m = 1; n = A->n; flag = 1; break;
    case 'c':
    case 'C':
      m = A->m; n = 1; flag = 2; break;
    }

  if (    (amin = nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT
       || (amax = nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT ) goto err;
  if ( lhs > 2 )
    if (    (imin = nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT
         || (imax = nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT ) goto err;

  
  if ( A->mn == 0 )  /* special case : quick return */
    { 
      *Amin = amin; *Amax = amax; *Imin = imin; *Imax = imax; 
      return OK; 
    }

  if ( flag == 0 )
    {
      VMiniMaxi(A->mn, A->R, 1, amin->R, amax->R, &indmin, &indmax);
      if (lhs > 2) {imin->R[0] = (double) indmin; imax->R[0] = (double) indmax;}
    }
  else if ( flag == 1 )
    for ( k = 0 ; k < A->n ; k++ )
      {
	VMiniMaxi(A->m, &(A->R[k*A->m]), 1, &(amin->R[k]), &(amax->R[k]), &indmin, &indmax);
	if (lhs > 2) {imin->R[k] = (double) indmin; imax->R[k] = (double) indmax;}
      }
  else
    for ( k = 0 ; k < A->m ; k++ )
      {
	VMiniMaxi(A->mn, &(A->R[k]), A->m, &(amin->R[k]), &(amax->R[k]), &indmin, &indmax);
	if (lhs > 2) {imin->R[k] = (double) indmin; imax->R[k] = (double) indmax;}
      }

  *Amin = amin; *Amax = amax; *Imin = imin; *Imax = imax; 
  return OK;

 err:
  nsp_matrix_destroy(amin); nsp_matrix_destroy(amax); 
  nsp_matrix_destroy(imin); nsp_matrix_destroy(imax);
  return FAIL;
}


/*
 * Creates a Matrix and initialize it with the 
 * function func 
 * R=func(i,j) or R=func(i,j,&Imag) 
 */

NspMatrix *nsp_mat_createinit(char *name, char type, int m, int n, double (*func) (/* ??? */))
{
  NspMatrix *Loc;
  int i1,i2;
  if (( Loc= nsp_matrix_create(name,type,m,n)) == NULLMAT) return(NULLMAT);
  if ( type == 'c') 
    {
      for (i2 = 0 ; i2 < n ; i2++)
	for (i1 = 0 ; i1 < m ; i1++)
	  { 
	    int mi2=m*i2;
	    double imag;
	    Loc->C[i1+mi2].r = (*func)(i1,i2,&imag);
	    Loc->C[i1+mi2].i = imag ;
	  };
    }
  else 
    {
      for (i2 = 0 ; i2 < n ; i2++)
	for (i1 = 0 ; i1 < m ; i1++)
	  { 
	    int mi2=m*i2;
	    Loc->R[i1+mi2] = (*func)(i1,i2);
	  };
    }
  return(Loc);
}



/*
 *nsp_mat_triu: A=Triu(A)
 * A is changed  
 */

void nsp_mat_triu(NspMatrix *A, int k)
{
  double d=0.00;
  int i,j;
  for ( i =0 ; i < A->m ; i++) 
    {
      for ( j = 0 ; j < Min(k+i,A->n) ; j++) 
	{
	  switch ( A->rc_type ) 
	    {
	    case 'r' : 
	      A->R[i+A->m*j] = d; break ;
	    case 'c' : 
	      A->C[i+A->m*j].r = d;
	      A->C[i+A->m*j].i = d;
	    }
	}
    }
}


/*
 *nsp_mat_tril: A=Tril(A)
 * A is changed  
 */

void nsp_mat_tril(NspMatrix *A, int k)
{
  double d=0.00;
  int i,j;
  for ( i =0 ; i < A->m ; i++) 
    {
      for ( j = Max(0,k+i+1) ; j < A->n ; j++) 
	{
	  switch ( A->rc_type ) 
	    {
	    case 'r' : 
	      A->R[i+A->m*j] = d; break ;
	    case 'c' : 
	      A->C[i+A->m*j].r = d;
	      A->C[i+A->m*j].i = d;
	    }
	}
    }
}


/*
 *nsp_mat_eye: A=Eye(m,n)
 */

NspMatrix *nsp_mat_eye(int m, int n)
{
  NspMatrix *Loc;
  int i;
  if (( Loc= nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT) return(NULLMAT);
  nsp_mat_set_rval(Loc,(double) 0.00);
  for ( i=0 ; i < Min(m,n) ; i++) Loc->R[i+m*i]= (double) 1.00;
  return(Loc);
}

/*
 *nsp_mat_ones: A=ones(m,n)
 * A is changed  
 */

NspMatrix *nsp_mat_ones(int m, int n)
{
  NspMatrix *Loc;
  if ((Loc= nsp_matrix_create(NVOID,'r',m,n))  == NULLMAT) return(NULLMAT);
  nsp_mat_set_rval(Loc,(double) 1.00);
  return(Loc);
}

/*
 *nsp_mat_ones: A=zeros(m,n)
 * A is changed  
 */

NspMatrix *nsp_mat_zeros(int m, int n)
{
  NspMatrix *Loc;
  if ((Loc= nsp_matrix_create(NVOID,'r',m,n))  == NULLMAT) return(NULLMAT);
  nsp_mat_set_rval(Loc,(double) 0.00);
  return(Loc);
}

/*
 *nsp_mat_rand: A=rand(m,n)
 * A is changed  
 */

static int rand_data[] = {1,0};

NspMatrix *nsp_mat_rand(int m, int n)
{
  NspMatrix *Loc;
  int i;
  /* test2DD();*/ /* XXXXX*/

  if (( Loc= nsp_matrix_create(NVOID,'r',m,n))  == NULLMAT) return(NULLMAT);
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

void nsp_set_urandseed(int m)
{
  rand_data[0] = Max(m,0);
}

int nsp_get_urandseed(void)
{
  return rand_data[0];
}

void nsp_set_urandtype(int m)
{
  rand_data[1]=m;
}

int nsp_get_urandtype(void)
{
  return rand_data[1];
}

/**
 * nsp_mat_pow_matscalar:
 * @A: a #NspMatrix
 * @B: a #NspMatrix with only one element (that is a scalar !) 
 * 
 * This routine computes @A^@B where @B must be a scalar (this must
 * be verified by the calling routine (*)). The matrix @A is modified 
 * and hold the final result (if OK is returned).  @A must be
 * square (if not FAIL is returned). When @A is not a scalar
 * this routine works only if @B is an integer (and when @B is
 * a negative integer, @A must be numerically invertible).
 * Added by Bruno. 
 * 
 * (*) The operation A^B is done with the generic interface int_mx_mopscal
 *     which branches to one of the 3 routines:
 *        1/ nsp_mat_pow_matscalar(A,B) if B is a scalar 
 *        2/ nsp_mat_pow_matmat(A,B), if neither A and B are scalar
 *        3/ nsp_mat_pow_scalarmat(B,A), if A is a scalar
 *
 * Return value: %OK or %FAIL
 **/

int nsp_mat_pow_matscalar(NspMatrix *A, NspMatrix *B) 
{
  int p, i, oddflag=0;
  int A_is_square = A->m==A->n;

  if ( ! A_is_square )
    {
      Scierror("Error:\t matrix must be square\n");
      return FAIL;
    }

  if ( A->m == 1 ) /* A is a scalar */
    return nsp_mat_pow_scalar(A,B);

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
 * nsp_mat_pow_matmat:
 * @A: a #NspMatrix which is not a scalar
 * @B: a #NspMatrix which is not a scalar
 * 
 * The operation A^B is done with the generic interface int_mx_mopscal
 * which branches to one of the 3 routines:
 *        1/ nsp_mat_pow_matscalar(A,B) if B is a scalar 
 *        2/ nsp_mat_pow_matmat(A,B), if neither A and B are scalar
 *        3/ nsp_mat_pow_scalarmat(B,A), if A is a scalar
 *
 * Here this routine is made for the case 2 but as it is not a defined
 * operation it displays only an error message.  
 *
 * Return value: FAIL
 **/
int nsp_mat_pow_matmat(NspMatrix *A, NspMatrix *B) 
{
  Scierror("Error:\t for ^ operator at least one operand must be a scalar\n");
  return FAIL;
}

/**
 * nsp_mat_pow_scalarmat:
 * @B: a #NspMatrix which must be square 
 * @A: a #NspMatrix which must be a scalar 
 *  
 * The operation A^B is done with the generic interface int_mx_mopscal
 * which branches to one of the 3 routines:
 *        1/ nsp_mat_pow_matscalar(A,B) if B is a scalar 
 *        2/ nsp_mat_pow_matmat(A,B), if neither A and B are scalar
 *        3/ nsp_mat_pow_scalarmat(B,A), if A is a scalar
 * caution B is overwritten with the resulting matrix.
 *
 * algorithm : A^B = e^(ln(A)*B), so we use expm( ln(A)*B )
 * 
 * Return value: OK or FAIL 
 **/
int nsp_mat_pow_scalarmat(NspMatrix *B, NspMatrix *A) 
{
  double a=0.0;
  doubleC ac, acc;
  char rc_flag;
  int i;
  NspMatrix *C; /* used because A must not be modified : C will contain the scalar ln(a) */

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

  if ( (C = nsp_matrix_create(NVOID,rc_flag,1,1)) == NULLMAT ) return FAIL;

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

  if ( nsp_mat_mult_scalar(B, C) == FAIL ) goto err;

  if ( nsp_expm(B) == FAIL ) goto err;

  nsp_matrix_destroy(C); 
  return OK;

 err:
  nsp_matrix_destroy(C); 
  return FAIL;
}


/*
 *  A Set of term to term function on Matrices (complex or real)
 */


/*
 * term to term power : the general case 
 * A = A .^ B (covers the scalar and [] cases ) 
 */

int nsp_mat_pow_tt(NspMatrix *A, NspMatrix *B) 
{
  return MatOp(A,B,nsp_mat_pow_scalar,MatNoOp,nsp_mat_pow_el,1);
}


int nsp_mat_pow_el(NspMatrix *A, NspMatrix *B)
{
  if (SameDim(A,B))
    {
      int i;
      if(A->rc_type == 'r' ) 
	{
	  if ( B->rc_type == 'r') 
	    {
	      for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = pow(A->R[i],B->R[i]);
	    }
	  else 
	    {
	      if (nsp_mat_set_ival(A,0.00) == FAIL ) return(FAIL);
	      for ( i = 0 ; i < A->mn ; i++ )nsp_pow_dc(A->C[i].r,&B->C[i],&A->C[i]);
	    }
	}
      else
	{
	  if ( B->rc_type == 'r') 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )nsp_pow_cd(&A->C[i],B->R[i],&A->C[i]);
	    }
	  else 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )nsp_pow_cc(&A->C[i],&B->C[i],&A->C[i]);
	    }
	}
      return(OK);
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
}

/* scalar case A = A .^ b */ 

int nsp_mat_pow_scalar(NspMatrix *A, NspMatrix *B)
{
  int i;
  if(A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r') 
	{
	  for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = pow(A->R[i],B->R[0]);
	}
      else 
	{
	  if (nsp_mat_set_ival(A,0.00) == FAIL ) return(FAIL);
	  for ( i = 0 ; i < A->mn ; i++ )nsp_pow_dc(A->C[i].r,&B->C[0],&A->C[i]);
	}
    }
  else
    {
      if ( B->rc_type == 'r') 
	{
	  for ( i = 0 ; i < A->mn ; i++ )nsp_pow_cd(&A->C[i],B->R[0],&A->C[i]);
	}
      else 
	{
	  for ( i = 0 ; i < A->mn ; i++ )nsp_pow_cc(&A->C[i],&B->C[0],&A->C[i]);
	}
    }
  return(OK);
}

/* scalar case  a(i,j)=b**a(i,j) */ 

int nsp_mat_pow_scalarm(NspMatrix *A, NspMatrix *B)
{
  int i;
  if(A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r') 
	{
	  for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = pow(B->R[0],A->R[i]);
	}
      else 
	{
	  if (nsp_mat_set_ival(A,0.00) == FAIL ) return(FAIL);
	  for ( i = 0 ; i < A->mn ; i++ )nsp_pow_cd(&B->C[0],A->C[i].r,&A->C[i]);
	}
    }
  else
    {
      if ( B->rc_type == 'r') 
	{
	  for ( i = 0 ; i < A->mn ; i++ )nsp_pow_dc(B->R[0],&A->C[i],&A->C[i]);
	}
      else 
	{
	  for ( i = 0 ; i < A->mn ; i++ )nsp_pow_cc(&B->C[i],&A->C[i],&A->C[0]);
	}
    }
  return(OK);
}


/*
 * term to term division : the general case 
 * A = A ./ B (covers the scalar and [] cases ) 
 */

int nsp_mat_div_tt(NspMatrix *A, NspMatrix *B) 
{
  return MatOp(A,B,nsp_mat_div_scalar,MatNoOp,nsp_mat_div_el,1);
}

/* deals with the case dim A == dim B **/

int nsp_mat_div_el(NspMatrix *A, NspMatrix *B)
{
  if (SameDim(A,B))
    {
      int i;
      if(A->rc_type == 'r' ) 
	{
	  if ( B->rc_type == 'r') 
	    {
	      for ( i = 0 ; i < A->mn ; i++ ) A->R[i] /= B->R[i];
	    }
	  else 
	    {
	      if (nsp_mat_complexify(A,0.00) == FAIL ) return(FAIL);
	      for ( i = 0 ; i < A->mn ; i++ )nsp_div_dc(A->C[i].r,&B->C[i],&A->C[i]);
	    }
	}
      else
	{
	  if ( B->rc_type == 'r') 
	    {
	      for ( i = 0 ; i < A->mn ; i++ ) 
		{
		  A->C[i].r /= B->R[i];
		  A->C[i].i /= B->R[i];
		}
	    }
	  else 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )nsp_div_cc(&A->C[i],&B->C[i],&A->C[i]);
	    }
	}
      return(OK);
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
}

/* A = A ./ b where b is scalar **/ 

int nsp_mat_div_scalar(NspMatrix *A, NspMatrix *B)
{
  int i;
  if(A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r') 
	{
	  for ( i = 0 ; i < A->mn ; i++ ) A->R[i] /= B->R[0];
	}
      else 
	{
	  if (nsp_mat_set_ival(A,0.00) == FAIL ) return(FAIL);
	  for ( i = 0 ; i < A->mn ; i++ )nsp_div_dc(A->C[i].r,&B->C[0],&A->C[i]);
	}
    }
  else
    {
      if ( B->rc_type == 'r') 
	{
	  for ( i = 0 ; i < A->mn ; i++ ) 
	    {
	      A->C[i].r /= B->R[0];
	      A->C[i].i /= B->R[0];
	    }
	}
      else 
	{
	  for ( i = 0 ; i < A->mn ; i++ )nsp_div_cc(&A->C[i],&B->C[0],&A->C[i]);
	}
    }
  return(OK);
}

/*
 * term to term bdivision : the general case 
 * A = A .\ B (covers the scalar and [] cases ) 
 */

int nsp_mat_bdiv_tt(NspMatrix *A, NspMatrix *B) 
{
  return MatOp(A,B,nsp_mat_bdiv_scalar,MatNoOp,nsp_mat_bdiv_el,1);
}

/* deals with the case dim A == dim B **/

int nsp_mat_bdiv_el(NspMatrix *A, NspMatrix *B)
{
  if (SameDim(A,B))
    {
      int i;
      if(A->rc_type == 'r' ) 
	{
	  if ( B->rc_type == 'r') 
	    {
	      for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = B->R[i]/A->R[i];
	    }
	  else 
	    {
	      if (nsp_mat_set_ival(A,0.00) == FAIL ) return(FAIL);
	      for ( i = 0 ; i < A->mn ; i++ )  
		{
		  A->C[i].r = B->C[i].r / A->C[i].r;
		  A->C[i].r = B->C[i].i / A->C[i].r;
		}
	    }
	}
      else
	{
	  if ( B->rc_type == 'r') 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )nsp_div_dc(B->R[i],&A->C[i],&A->C[i]);
	    }
	  else 
	    {
	      for ( i = 0 ; i < A->mn ; i++ )nsp_div_cc(&B->C[i],&A->C[i],&A->C[i]);
	    }
	}
      return(OK);
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
}

/* A = A .\ b where b is scalar **/ 

int nsp_mat_bdiv_scalar(NspMatrix *A, NspMatrix *B)
{
  int i;
  if(A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r') 
	{
	  for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = B->R[0]/A->R[i];
	}
      else 
	{
	  if (nsp_mat_set_ival(A,0.00) == FAIL ) return(FAIL);
	  for ( i = 0 ; i < A->mn ; i++ )  
	    {
	      A->C[i].r = B->C[0].r / A->C[i].r;
	      A->C[i].r = B->C[0].i / A->C[i].r;
	    }
	}
    }
  else
    {
      if ( B->rc_type == 'r') 
	{
	  for ( i = 0 ; i < A->mn ; i++ )nsp_div_dc(B->R[0],&A->C[i],&A->C[i]);
	}
      else 
	{
	  for ( i = 0 ; i < A->mn ; i++ )nsp_div_cc(&B->C[0],&A->C[i],&A->C[i]);
	}
    }
  return(OK);
}

/*
 * term to term multiplication : the general case 
 * A = A .* B (covers the scalar and [] cases ) 
 */

int nsp_mat_mult_tt(NspMatrix *A, NspMatrix *B) 
{
  return MatOp(A,B,nsp_mat_mult_scalar,MatNoOp,nsp_mat_mult_el,1);
}

/* 
 *  deals with the case dim A == dim B plus the special following cases
 *  which add the possibility to compute efficiently with a diag matrix
 *  represented by a vector (Bruno, sept 8 2005) by adding 2 new rules
 *  for the .* operator :
 *      1/ In place of diag(A)*B we do A.*B (if B is m x n, A must be m x 1)
 *      2/ In place of A*diag(B) we do A.*B (if A is m x n, B must be 1 x n)
 */
int nsp_mat_mult_el(NspMatrix *A, NspMatrix *B)
{
  if (SameDim(A,B))
    {
      int i;
      if(A->rc_type == 'r' ) 
	{
	  if ( B->rc_type == 'r') 
	    for ( i = 0 ; i < A->mn ; i++ ) A->R[i] *= B->R[i];
	  else 
	    {
	      if (nsp_mat_complexify(A,0.00) == FAIL ) return FAIL;
	      for ( i = 0 ; i < A->mn ; i++ )  
		{
		  A->C[i].i = A->C[i].r * B->C[i].i ;
		  A->C[i].r *= B->C[i].r ;
		}
	    }
	}
      else
	{
	  if ( B->rc_type == 'r') 
	    for ( i = 0 ; i < A->mn ; i++ ) 
	      {
		A->C[i].r *= B->R[i];
		A->C[i].i *= B->R[i];
	      }
	  else 
	    for ( i = 0 ; i < A->mn ; i++ )nsp_prod_c(&A->C[i],&B->C[i]);
	}
      return OK;
    }
  else if ( A->m == B->m  &&  A->n == 1 )    /* diag(A)*B */
    {
      /* result must be in A so copy A first in coef then resize A to the sizes of B */
      NspMatrix *coef;
      int i,j, k=0;
      if ( (coef=nsp_matrix_copy(A)) == NULLMAT ) return FAIL;
      if ( nsp_matrix_resize(A,B->m,B->n) == FAIL ) {nsp_matrix_destroy(coef); return FAIL;}

      if(A->rc_type == 'r' ) 
	{
	  if ( B->rc_type == 'r') 
	    {
	      for ( j = 0 ; j < A->n ; j++)
		for ( i = 0 ; i < A->m ; i++, k++ ) A->R[k] = B->R[k]*coef->R[i];
	    }
	  else 
	    {
	      if (nsp_mat_complexify(A,0.00) == FAIL ) {nsp_matrix_destroy(coef); return FAIL;}
	      for ( j = 0 ; j < A->n ; j++)
		for ( i = 0 ; i < A->m ; i++, k++ ) 
		  {
		    A->C[k].r = B->C[k].r * coef->R[i];
		    A->C[k].i = B->C[k].i * coef->R[i];
		  }
	    }
	}
      else
	{
	  if ( B->rc_type == 'r') 
	    for ( j = 0 ; j < A->n ; j++)
	      for ( i = 0 ; i < A->m ; i++, k++ ) 
		{
		  A->C[k].r = B->R[k] * coef->C[i].r;
		  A->C[k].i = B->R[k] * coef->C[i].i;;
		}
	  else 
	    for ( j = 0 ; j < A->n ; j++)
	      for ( i = 0 ; i < A->m ; i++, k++ ) 
		{
		  A->C[k].r = B->C[k].r * coef->C[i].r -  B->C[k].i * coef->C[i].i;
		  A->C[k].i = B->C[k].r * coef->C[i].i +  B->C[k].i * coef->C[i].r;
		}
	}
      nsp_matrix_destroy(coef);
      return OK;
    }
  else if ( A->n == B->n  &&  B->m == 1 )    /* A*diag(B) */
    {
      int i,j, k=0;
      if(A->rc_type == 'r' ) 
	{
	  if ( B->rc_type == 'r') 
	    {
	      for ( j = 0 ; j < A->n ; j++)
		for ( i = 0 ; i < A->m ; i++, k++ ) A->R[k] *= B->R[j];
	    }
	  else 
	    {
	      if (nsp_mat_complexify(A,0.00) == FAIL ) return FAIL;
	      for ( j = 0 ; j < A->n ; j++)
		for ( i = 0 ; i < A->m ; i++, k++ ) 
		  {
		    A->C[k].i = A->C[k].r * B->C[j].i;
		    A->C[k].r = A->C[k].r * B->C[j].r;
		  }
	    }
	}
      else
	{
	  if ( B->rc_type == 'r') 
	    for ( j = 0 ; j < A->n ; j++)
	      for ( i = 0 ; i < A->m ; i++, k++ ) 
		{
		  A->C[k].r *= B->R[j];
		  A->C[k].i *= B->R[j];
		}
	  else 
	    for ( j = 0 ; j < A->n ; j++)
	      for ( i = 0 ; i < A->m ; i++, k++ ) 
		nsp_prod_c(&A->C[k],&B->C[j]);
	}
      return OK;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return FAIL;
    }
}

/* the scalar case Mat2 is assumed scalar */
 
int nsp_mat_mult_scalar(NspMatrix *Mat1, NspMatrix *Mat2)
{
  return MatOpScalar(Mat1,Mat2,nsp_dvmul,nsp_zvmul);
}



/*
 * A=Acos(A), 
 * A is changed 
 */

int nsp_mat_acos(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= acos(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++)nsp_acos_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}

/*
 * A=Acosh(A), 
 * A is changed 
 */

int nsp_mat_acosh(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= acosh(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++)nsp_acosh_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}


/*
 * A=Asin(A), 
 * A is changed 
 */

int nsp_mat_asin(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) 
	{ 
	  if ( A->R[i] > 1.0 || A->R[i] < -1.0) 
	    {
	      int j;
	      /* switch to complex */ 
	      if (nsp_mat_complexify(A,0.0) == FAIL ) 
		return(FAIL);
	      for (j = i ; j < A->mn ; j++)nsp_asin_c(&A->C[j],&A->C[j]);
	      return OK;
	    }
	  else 
	    A->R[i]= asin(A->R[i]);
	}
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++)nsp_asin_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}


/*
 * A=Asinh(A),
 * A is changed 
 */

int nsp_mat_asinh(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= asinh(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++)nsp_asinh_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}


/*
 * A=Atan(A),  * A is changed 
 */

int nsp_mat_atan(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= atan(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++)nsp_atan_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}


/*
 * A=Atan2(A,B),  * A is changed 
 */

int nsp_mat_atan2(NspMatrix *A,NspMatrix *B)
{
  int i ;
  if ( A->rc_type == 'r' && B->rc_type == 'r' ) 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= atan2(A->R[i],B->R[i]);
      return(OK);
    }
  return FAIL;
}


/*
 * A=Atanh(A),
 * A is changed 
 */

int nsp_mat_atanh(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= atanh(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++)nsp_atanh_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}


/*
 *nsp_mat_ceil: A=Ceil(A)
 * A is changed  
 */

void nsp_mat_ceil(NspMatrix *A)
{
  int i ;
  switch ( A->rc_type ) 
    {
    case 'r' : 
      for ( i=0 ; i < A->mn ; i++) A->R[i]=ceil(A->R[i]); 
      break ;
    case 'c' :
      for ( i=0 ; i < A->mn ; i++)nsp_ceil_c(&A->C[i],&A->C[i]);
      break;
    }

}

/*
 *nsp_mat_modulo: A=nsp_mat_modulo(A,n)
 * A is changed to A / n : remainder of int division. 
 */

void nsp_mat_modulo(NspMatrix *A, int n)
{
  int i ;
  nsp_mat_int(A);
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

/*
 *nsp_mat_idiv: A=nsp_mat_idiv(A,n)
 * A is changed to A / n :  quotient in int division
 */

void nsp_mat_idiv(NspMatrix *A, int n)
{
  int i ;
  nsp_mat_int(A);
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


/*
 *nsp_mat_int: A=Int(A)
 * A is changed  
 */

void nsp_mat_int(NspMatrix *A)
{
  int i ;
  switch ( A->rc_type ) 
    {
    case 'r' : 
      for ( i=0 ; i < A->mn ; i++) A->R[i]=aint(A->R[i]); 
      break ;
    case 'c' :
      for ( i=0 ; i < A->mn ; i++)nsp_aint_c(&A->C[i],&A->C[i]);
      break;
    }
}


/*
 *nsp_mat_floor: A=Floor(A)
 * A is changed  
 */

void nsp_mat_floor(NspMatrix *A)
{
  int i ;
  switch ( A->rc_type ) 
    {
    case 'r' : 
      for ( i=0 ; i < A->mn ; i++) A->R[i]=floor(A->R[i]); 
      break ;
    case 'c' :
      for ( i=0 ; i < A->mn ; i++)nsp_floor_c(&A->C[i],&A->C[i]);
      break;
    }

}

/*
 *nsp_mat_round: A=Round(A)
 * A is changed  
 */

void nsp_mat_round(NspMatrix *A)
{
  int i ;
  switch ( A->rc_type ) 
    {
    case 'r' :
      for ( i=0 ; i < A->mn ; i++) A->R[i]=anint(A->R[i]);
      break ;
    case  'c':
      for ( i=0 ; i < A->mn ; i++)nsp_round_c(&A->C[i],&A->C[i]);
      break;
    }
}

/*
 *nsp_mat_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */

int nsp_mat_sign(NspMatrix *A)
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


/*
 * A=Tan(A), absolue value or module of each element 
 * A is changed 
 */

int nsp_mat_tan(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= tan(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++)nsp_tan_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}

/*
 * A=Tanh(A), absolue value or module of each element 
 * A is changed 
 */

int nsp_mat_tanh(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= tanh(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++)nsp_tanh_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}


/*
 * A=Abs(A), absolue value or module of each element 
 * A is changed 
 */

int nsp_mat_abs(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= fabs(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++) A->C[i].r=nsp_abs_c(&A->C[i]);
      if (nsp_mat_get_real(A) == FAIL) return(FAIL);
    }
  return(OK);
}

/*
 * A=Erf(A), Erf function 
 */

int nsp_mat_erf(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= erf(A->R[i]);
    }
  else
    {
      Scierror("Erf function argument must be real\n");
      return(FAIL);
    }
  return(OK);
}

/*
 * A=Erfc(A), Erf function 
 */

int nsp_mat_erfc(NspMatrix *A)
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

/*
 * A=gamma(A),
 */

int nsp_mat_tgamma(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
#ifdef HAVE_TGAMMA
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= tgamma(A->R[i]);
#else 
      Scierror("Error: tgamma function not implemented\n");
      return(FAIL);
#endif 
    }
  else
    {
      Scierror("Error: tgamma function not implemented for complex\n");
      return(FAIL);
    }
  return(OK);
}

/*
 * A=log(gamma(A)), 
 */

int nsp_mat_lgamma(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
#ifdef HAVE_LGAMMA
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= lgamma(A->R[i]);
#else 
      Scierror("Error: lgamma function not implemented\n");
      return(FAIL);
#endif
    }
  else
    {
      Scierror("Error: lgamma function not implemented for complex\n");
      return(FAIL);
    }
  return(OK);
}



/*
 * A=Arg(A),
 * Argument or Phase 
 */

int nsp_mat_arg(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i] = 0.00;
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++) A->C[i].r =nsp_arg_c(&A->C[i]);
      if (nsp_mat_get_real(A)==FAIL) return(FAIL);
    }
  return(OK);
}


/*
 * A=Polar(A,B),
 * A=A(cos(B)+%i*sin(B);
 */


int nsp_mat_polar(NspMatrix *A, NspMatrix *B)
{
  int i ;
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) 
    {
      Scierror("MatPolar: arguments should be real\n");
      return(FAIL);
    }
  if (nsp_mat_complexify(A,0.00) == FAIL ) return(FAIL);
  for ( i = 0 ; i < A->mn ; i++)nsp_polar_c(A->C[i].r,B->R[i],&A->C[i]);
  return(OK);
}

/*
 * A= A & B logical int &  
 */

int nsp_mat_iand(NspMatrix *A, NspMatrix *B)
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
 * nsp_mat_iandu:
 * @A: 
 * @res: 
 * 
 * logical and of all the entries of A 
 * casted to int 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_mat_iandu(NspMatrix *A, unsigned int *res)
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

/*
 * A= A | B logical int or 
 */

int nsp_mat_ior(NspMatrix *A, NspMatrix *B)
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

/*
 * A= A << i  
 *      >> i 
 */

int nsp_mat_ishift(NspMatrix *A,int shift,char dir)
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
 * nsp_mat_ioru:
 * @A: 
 * @res: 
 * 
 * logical or of the entries of @A 
 * casted to int 
 * 
 * Return value: 
 **/

int nsp_mat_ioru(NspMatrix *A, unsigned int *res)
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

/*
 * nsp_mat_conj: A=real(A)-i*Imag(A)
 * A is changed  only if imaginary 
 */

void nsp_mat_conj(NspMatrix *A)
{
  int i=1;
  switch ( A->rc_type ) 
    {
    case 'r' : break;
    case 'c' : for ( i = 0 ; i < A->mn ; i++) A->C[i].i = - A->C[i].i;
      break;
    }
}

/*
 *nsp_mat_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */

void nsp_mat_cos(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r')
    {
      for ( i=0 ; i < A->mn ; i++) A->R[i] = cos(A->R[i]);
    }
  else
    {
      for ( i=0 ; i < A->mn ; i++)nsp_cos_c(&A->C[i],&A->C[i]);
    }
}

/*
 *nsp_mat_cos: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */

void nsp_mat_cosh(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r')
    {
      for ( i=0 ; i < A->mn ; i++) A->R[i] = cosh(A->R[i]);
    }
  else
    {
      for ( i=0 ; i < A->mn ; i++)nsp_cosh_c(&A->C[i],&A->C[i]);
    }
}

/*
 * MatExpl : Exponentiation term to term 
 * A is changed 
 */

void nsp_mat_expel(NspMatrix *A)
{
  int i;
  if(A->rc_type == 'r' ) 
    {
      for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = exp(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++ )nsp_exp_c(&A->C[i],&A->C[i]);
    }
}



/*
 * MatLog : A=LogEl(A)  log term to term 
 * A is changed  
 * The real case is special since the result can be complex
 */

int nsp_mat_logel(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r')
    {
      /* Check if really real or imaginary case */
      int itr = 0;
      for ( i=0 ; i < A->mn ; i++) 
	{
	  if ( A->R[i] < 0.00 ) 
	    {
	      itr = 1; break;
	    }
	}
      if ( itr == 0) 
	{
	  /* real case */
	  for ( i=0 ; i < A->mn ; i++) A->R[i] = log(A->R[i]);
	}
      else 
	{
	  /* realto imaginary imaginary case */
	  double d=00;
	  if (nsp_mat_set_ival(A,d) == FAIL ) return(FAIL);
	  for ( i=0 ; i < A->mn ; i++)nsp_log_c(&A->C[i],&A->C[i]);
	}
    }
  else
    {
      /* imaginary case **/
      for ( i=0 ; i < A->mn ; i++)nsp_log_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}




/*
 *nsp_mat_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

void nsp_mat_sin(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r')
    {
      for ( i=0 ; i < A->mn ; i++) A->R[i] = sin(A->R[i]);
    }
  else
    {
      for ( i=0 ; i < A->mn ; i++)nsp_sin_c(&A->C[i],&A->C[i]);
    }
}



/*
 *nsp_mat_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */

void nsp_mat_sinh(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r')
    {
      for ( i=0 ; i < A->mn ; i++) A->R[i] = sinh(A->R[i]);
    }
  else
    {
      for ( i=0 ; i < A->mn ; i++)nsp_sinh_c(&A->C[i],&A->C[i]);
    }
}


/*
 *nsp_mat_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 */

int nsp_mat_sqrtel(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r')
    {
      /* Check if really real or imaginary case */
      int itr = 0;
      for ( i=0 ; i < A->mn ; i++) 
	{
	  if ( A->R[i] < 0.00 ) 
	    {
	      itr = 1; break;
	    }
	}
      if ( itr == 0) 
	{
	  /* real case */
	  for ( i=0 ; i < A->mn ; i++) A->R[i] = sqrt(A->R[i]);
	}
      else 
	{
	  /* imaginary case */
	  double d=00;
	  if (nsp_mat_set_ival(A,d) == FAIL ) return(FAIL);
	  for ( i=0 ; i < A->mn ; i++)nsp_sqrt_c(&A->C[i],&A->C[i]);
	}
    }
  else
    {
      for ( i=0 ; i < A->mn ; i++)nsp_sqrt_c(&A->C[i],&A->C[i]);
    }
  return(OK);
}





/*
 *nsp_mat_minus(A),  A= -A 
 * A is changed 
 */

int nsp_mat_minus(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= - A->R[i];
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++) 
	{
	  A->C[i].i=  - A->C[i].i;
	  A->C[i].r=  - A->C[i].r;
	}
    }
  return(OK);
}

/*
 * nsp_mat_minus_maxplus(A),  A= -A 
 * except for - %inf which is not changed 
 * A is changed 
 */

int nsp_mat_minus_maxplus(NspMatrix *A)
{
  int i ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) 
	if ( isinf( A->R[i]) != -1  )  A->R[i] = - A->R[i];
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++) 
	{
	  if ( isinf( A->C[i].r) != -1 ) A->C[i].i=  - A->C[i].i;
	  if ( isinf( A->C[i].i) != -1 ) A->C[i].r=  - A->C[i].r;
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

static void Kronecker(NspMatrix *A, NspMatrix *B, NspMatrix *PK)
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
		      memcpy(DI, &B->C[k2],B->m*sizeof(double));
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
 *nsp_mat_magic: A=Magic(n)
 */


NspMatrix *nsp_mat_magic(int n)
{
  NspMatrix *Loc;
  if (( Loc = nsp_matrix_create(NVOID,'r',n,n))  == NULLMAT) return(NULLMAT);
  if ( Loc->mn != 0) 
    {
#if 0 
      C2F(magic)(Loc->R,&n,&n);
#else 
      nsp_magic_matrix_fill(Loc->R,n);
#endif 
    }
  return(Loc);
}

/*
 *nsp_mat_franck: A=Franck(n)
 */

NspMatrix *nsp_mat_franck(int n, int job)
{
  NspMatrix *Loc;
  if (( Loc = nsp_matrix_create(NVOID,'r',n,n)) == NULLMAT) return(NULLMAT);
  if ( Loc->mn != 0) 
    {
      if ( job == 0 ) 
	nsp_franck_matrix(Loc->R,n);
      else 
	nsp_franck_inverse_matrix(Loc->R,n);
    }
  return(Loc);
}

/*
 *nsp_mat_hilbert: A=Hilbert(n)
 */

NspMatrix *nsp_mat_hilbert(int n,int job)
{
  NspMatrix *Loc;
  if (( Loc = nsp_matrix_create(NVOID,'r',n,n))== NULLMAT) return(NULLMAT);
  if ( Loc->mn != 0) 
    {
      if ( job == 0 ) 
	nsp_hilbert_matrix(Loc->R,n);
      else 
	nsp_hilbert_inverse_matrix(Loc->R,n);
    }
  return(Loc);
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

static int C_Lt(doubleC *a, doubleC *b) {  return(nsp_abs_c(a)< nsp_abs_c(b));}
static int C_Le(doubleC *a, doubleC *b) {  return(nsp_abs_c(a)<=nsp_abs_c(b));}
static int C_Eq(doubleC *a, doubleC *b) {  return(a->r ==b->r && a->i == b->i );}
static int C_NEq(doubleC *a, doubleC *b) {  return(a->r != b->r || a->i != b->i );}
static int C_Gt(doubleC *a, doubleC *b) {  return(nsp_abs_c(a)> nsp_abs_c(b));}
static int C_Ge(doubleC *a, doubleC *b) {  return(nsp_abs_c(a)>=nsp_abs_c(b));}

typedef int (CompOp) (double,double);
typedef int (C_CompOp) (doubleC *,doubleC *);

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
  {">",Gt  , Le,C_Gt  , C_Le},
  {">=",Ge  , Lt,C_Ge  , C_Lt},
  {(char *) NULL, 0,0,0,0}
};

static int SearchComp(char *op, CompOp (**realop), C_CompOp (**C_realop))
{
  int i=0;
  while ( comptab[i].name != (char *) NULL)
    {
      int j;
      j = strcmp(op,comptab[i].name);
      if ( j == 0 )
	{
	  *realop = comptab[i].foncop;
	  *C_realop = comptab[i].C_foncop;
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


/*
 * Isinf 
 */

NspBMatrix  *nsp_mat_isinf(NspMatrix *A)
{
  int i;
  NspBMatrix *Loc ;
  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
  if ( Loc == NULLBMAT)  return(NULLBMAT);
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++ ) Loc->B[i]= isinf(A->R[i]);
    }
  else 
    {
      for ( i = 0 ; i < A->mn ; i++ ) Loc->B[i]= isinf(A->C[i].r) | isinf(A->C[i].i);
    }
  return(Loc);
}

/*
 * Isnan
 */

NspBMatrix  *nsp_mat_isnan(NspMatrix *A)
{
  int i;
  NspBMatrix *Loc ;
  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
  if ( Loc == NULLBMAT)  return(NULLBMAT);
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++ ) Loc->B[i]= isnan(A->R[i]);
    }
  else 
    {
      for ( i = 0 ; i < A->mn ; i++ ) Loc->B[i]= isnan(A->C[i].r) | isnan(A->C[i].i);
    }
  return(Loc);
}

/*
 * Isinf 
 */

NspBMatrix  *nsp_mat_finite(NspMatrix *A)
{
  int i;
  NspBMatrix *Loc ;
  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
  if ( Loc == NULLBMAT)  return(NULLBMAT);
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++ ) Loc->B[i]= finite(A->R[i]);
    }
  else 
    {
      for ( i = 0 ; i < A->mn ; i++ ) Loc->B[i]= finite(A->C[i].r) & finite(A->C[i].i);
    }
  return(Loc);
}


/*
 * Operation on Matrices leading to Boolean Matrices results 
 * Res = A(i,j) op B(i;j) 
 * with the special case : 
 *      A(i;j)op B(0,0) or A(0,0) op B(i,j) if A or B are of size 1x1
 *      
 * A and B are unchanged : Res is created 
 */

NspBMatrix  *nsp_mat_comp(NspMatrix *A, NspMatrix *B, char *op)
{
  CompOp *realop;
  C_CompOp *C_realop;
  int i;
  NspBMatrix *Loc ;
  if ( SearchComp(op,&realop,&C_realop) == FAIL) return(NULLBMAT);
  if ( !( A->m == B->m && A->n == B->n ) )
    {
      if ( B->mn == 1 && A->mn != 0 ) 
	{
	  /* Special case B is a constant, Loc created with true */
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) { return(NULLBMAT);   }
	  if ( A->rc_type == 'r') 
	    {
	      if ( B->rc_type == 'r' ) 
		{
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*realop)(A->R[i],B->R[0]) ) Loc->B[i] = FALSE;
		}
	      else 
		{
		  NspMatrix *LocA ; 
		  if ( ( LocA =nsp_matrix_copy(A)) == NULLMAT) return NULLBMAT;
		  if (nsp_mat_complexify(LocA,0.00) == FAIL ) return NULLBMAT;
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&LocA->C[i],&B->C[0])) Loc->B[i] = FALSE;
		  nsp_matrix_destroy(LocA);
		}
	    }
	  else 
	    {
	      if ( B->rc_type == 'r' ) 
		{
		  doubleC Z={ B->R[0],0.0};
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&Z) ) Loc->B[i] = FALSE;
		}
	      else 
		{
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&B->C[0]) ) Loc->B[i] = FALSE;
		}
	    }
	  return(Loc);
	}
      if ( A->mn == 1 && B->mn != 0 ) 
	{
	  /* Special case A is a constant */
	  Loc =nsp_bmatrix_create(NVOID,B->m,B->n);
	  if ( Loc == NULLBMAT)     { return(NULLBMAT);  }
	  if ( A->rc_type == 'r' ) 
	    {
	      if ( B->rc_type == 'r' ) 
		{
		  for ( i = 0 ; i < B->mn ; i++ )  
		    if ( (*realop)(A->R[0],B->R[i]) ) Loc->B[i] = FALSE;
		}
	      else
		{
		  doubleC Z={ A->R[0],0.0};
		  for ( i = 0 ; i < B->mn ; i++ )  
		    if ( (*C_realop)(&Z,&B->C[i]) ) Loc->B[i] = FALSE;
		}
	    }
	  else
	    {
	      if ( B->rc_type == 'r' ) 
		{
		  NspMatrix *LocB ; 
		  if ( ( LocB =nsp_matrix_copy(B)) == NULLMAT) return NULLBMAT;
		  if (nsp_mat_complexify(LocB,0.00) == FAIL ) return NULLBMAT;
		  for ( i = 0 ; i < B->mn ; i++ )  
		    if ( (*C_realop)(&A->C[0],&B->C[i])) Loc->B[i] = FALSE;
		}
	      else
		{
		  for ( i = 0 ; i < B->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&B->C[i]) ) Loc->B[i] = FALSE;
		}
	    }
	  return(Loc);
	}
      /* Incompatible dimensions */
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
	  /* this is the scilab way ! I prefer to return %f as for == 
	  if ( ( B->mn == 1 && A->mn == 0 ) || ( A->mn == 1 && B->mn == 0 ) )
	    {
	      if ((Loc =nsp_bmatrix_create(NVOID,0,0))== NULLBMAT)return(NULLBMAT);
	      return Loc;
	    }
	  else 
	    {
	      Scierror("Error:\tIncompatible dimensions\n");
	      return( NULLBMAT);
	    }
	  */
	}
    }
  else 
    {
      /* A and B are of same dimensions */
      if ( A->mn == 0) 
	{
	  Loc =nsp_bmatrix_create(NVOID,1,1);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  if ( (*realop)(1.0,1.0)) Loc->B[0] = FALSE;
	}
      else
	{
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  if ( A->rc_type == 'r' )
	    {
	      if ( B->rc_type == 'r') 
		{
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*realop)(A->R[i],B->R[i])) Loc->B[i] = FALSE;
		}
	      else
		{
		  NspMatrix *LocA ; 
		  if ( ( LocA =nsp_matrix_copy(A)) == NULLMAT) return NULLBMAT;
		  if (nsp_mat_complexify(LocA,0.00) == FAIL ) return  NULLBMAT;
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&LocA->C[i],&B->C[i])) Loc->B[i] = FALSE;
		  nsp_matrix_destroy(LocA);
		}
	    }
	  else 
	    {
	      if ( B->rc_type == 'r') 
		{
		  NspMatrix *LocB ; 
		  if ( ( LocB =nsp_matrix_copy(B)) == NULLMAT) return NULLBMAT;
		  if (nsp_mat_complexify(LocB,0.00) == FAIL ) return NULLBMAT;
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&LocB->C[i])) Loc->B[i] = FALSE;
		  nsp_matrix_destroy(LocB);
		}
	      else
		{
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&B->C[i])) Loc->B[i] = FALSE;
		}
	    }
	}
    }
  return(Loc);
}

/* 
 * returns and(nsp_mat_comp(A,B,op))
 * err is set to TRUE if an  allocation error is raised or if 
 * op is not found 
 */ 


int nsp_mat_fullcomp(NspMatrix *A, NspMatrix *B, char *op,int *err)
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
	  if ( A->rc_type == 'r') 
	    {
	      if ( B->rc_type == 'r' ) 
		{
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*realop)(A->R[i],B->R[0]) ) return FALSE;
		}
	      else 
		{
		  NspMatrix *LocA ; 
		  if ( ( LocA =nsp_matrix_copy(A)) == NULLMAT) { *err=TRUE; return FALSE;}
		  if (nsp_mat_complexify(LocA,0.00) == FAIL ) { *err=TRUE; return FALSE;}
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&LocA->C[i],&B->C[0])) 
		      {
			nsp_matrix_destroy(LocA);
			return FALSE;
		      }
		  nsp_matrix_destroy(LocA);
		}
	    }
	  else 
	    {
	      if ( B->rc_type == 'r' ) 
		{
		  doubleC Z={ B->R[0],0.0};
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&Z) ) return FALSE;
		}
	      else 
		{
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&B->C[0]) ) return FALSE;
		}
	    }
	  return(Loc);
	}
      if ( A->mn == 1 && B->mn != 0 ) 
	{
	  /* Special case A is a constant */
	  if ( A->rc_type == 'r' ) 
	    {
	      if ( B->rc_type == 'r' ) 
		{
		  for ( i = 0 ; i < B->mn ; i++ )  
		    if ( (*realop)(A->R[0],B->R[i]) ) return FALSE;
		}
	      else
		{
		  doubleC Z={ A->R[0],0.0};
		  for ( i = 0 ; i < B->mn ; i++ )  
		    if ( (*C_realop)(&Z,&B->C[i]) ) return  FALSE;
		}
	    }
	  else
	    {
	      if ( B->rc_type == 'r' ) 
		{
		  NspMatrix *LocB ; 
		  if ( ( LocB =nsp_matrix_copy(B)) == NULLMAT) { *err=TRUE; return FALSE;}
		  if (nsp_mat_complexify(LocB,0.00) == FAIL ) { *err=TRUE; return FALSE;} 
		  for ( i = 0 ; i < B->mn ; i++ )  
		    if ( (*C_realop)(&A->C[0],&B->C[i])) 
		      {
			nsp_matrix_destroy(LocB);
			return  FALSE;
		      }
		  nsp_matrix_destroy(LocB);

		}
	      else
		{
		  for ( i = 0 ; i < B->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&B->C[i]) ) return FALSE;
		}
	    }
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
	  /* this is the scilab way ! I prefer to return %f as for == 
	  if ( ( B->mn == 1 && A->mn == 0 ) || ( A->mn == 1 && B->mn == 0 ) )
	    {
	      if ((Loc =nsp_bmatrix_create(NVOID,0,0))== NULLBMAT) { *err=TRUE; return FALSE;} 
	      return Loc;
	    }
	  else 
	    {
	      Scierror("Error:\tIncompatible dimensions\n");
	      return FALSE;
	    }
	  */
	}
    }
  else 
    {
      /* A and B are of same dimensions */
      if ( A->mn == 0) 
	{
	  if ( (*realop)(1.0,1.0)) return  FALSE;
	}
      else
	{
	  if ( A->rc_type == 'r' )
	    {
	      if ( B->rc_type == 'r') 
		{
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*realop)(A->R[i],B->R[i])) return FALSE;
		}
	      else
		{
		  NspMatrix *LocA ; 
		  if ( ( LocA =nsp_matrix_copy(A)) == NULLMAT) { *err=TRUE; return FALSE;}
		  if (nsp_mat_complexify(LocA,0.00) == FAIL ) { *err=TRUE; return FALSE;} 
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&LocA->C[i],&B->C[i])) 
		      {
			nsp_matrix_destroy(LocA);
			return FALSE;
		      }
		  nsp_matrix_destroy(LocA);
		}
	    }
	  else 
	    {
	      if ( B->rc_type == 'r') 
		{
		  NspMatrix *LocB ; 
		  if ( ( LocB =nsp_matrix_copy(B)) == NULLMAT) { *err=TRUE; return FALSE;}
		  if (nsp_mat_complexify(LocB,0.00) == FAIL ) { *err=TRUE; return FALSE;} 
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&LocB->C[i])) 
		      {
			nsp_matrix_destroy(LocB);
			return FALSE;
		      }
		  nsp_matrix_destroy(LocB);
		}
	      else
		{
		  for ( i = 0 ; i < A->mn ; i++ )  
		    if ( (*C_realop)(&A->C[i],&B->C[i])) return FALSE;
		}
	    }
	}
    }
  return TRUE;
}


/*
 * returns in a NspMatrix the indices for which the 
 * Matrix A has non zero entries 
 * A is left unchanged
 * according to lhs one or two arguments are returned 
 */

int nsp_mat_find(NspMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2)
{
  int j,i,count=0;
  /* first pass for counting **/
  for ( i=0 ; i < A->mn ; i++) 
    {
      if ( A->rc_type == 'r' && A->R[i] != 0.0 ) count++;
      if ( A->rc_type == 'c' && (A->C[i].r != 0.0 || A->C[i].i != 0.0))
	count++;
    }
  if ( lhs == 1) 
    {
      *Res1 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res1 == NULLMAT) return FAIL;
      count=0;
      for ( i = 0 ; i < A->mn ; i++ )
	{
	  if ( A->rc_type == 'r' && A->R[i] != 0.0 ) 
	    (*Res1)->R[count++] = i+1;
	  if ( A->rc_type == 'c' && (A->C[i].r != 0.0 || A->C[i].i != 0.0))
	    (*Res1)->R[count++] = i+1;
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
	for ( j = 0 ; j < A->n ; j++ )
	  {
	    if ( A->rc_type == 'r' && A->R[i+(A->m)*j] != 0.0 ) 
	      {
		(*Res1)->R[count] = i+1;
		(*Res2)->R[count++] = j+1;
	      }
	    if ( A->rc_type == 'c' && (A->C[i+(A->m)*j].r != 0.0 || A->C[i+(A->m)*j].i != 0.0))
	      {
		(*Res1)->R[count] = i+1;
		(*Res2)->R[count++] = j+1;
	      }
	  }
    }
  return OK;
}



/*
 * Res=nsp_mpmat_mult(A,B) matrix product in max plus algebra 
 * 
 */

#define PLUS(x,y) (( isinf( x ) == -1  || isinf( y ) == -1 ) ? (Min(x,y)) : x+y )

NspMatrix *nsp_mat_maxplus_mult(NspMatrix *A, NspMatrix *B)
{  
  NspMatrix *Loc;
  if ( A->n != B->m ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(NULLMAT);
    }
  if ( A->rc_type == 'c' ) 
    {
      if ( B->rc_type == 'r' ) 
	{
	  if (nsp_mat_set_ival(B,0.00) == FAIL ) return(NULLMAT);
	}
    }
  else 
    { 
      if ( B->rc_type == 'c' ) 
	{
	  if (nsp_mat_set_ival(A,0.00) == FAIL ) return(NULLMAT);
	}
    }
  if ((Loc = nsp_matrix_create(NVOID,A->rc_type,A->m,B->n))==  NULLMAT) return(NULLMAT);
  if ( Loc->rc_type == 'c' ) 
    {
      int i,j;
      for ( i = 0 ; i < A->m ; i++) 
	for ( j = 0 ; j < B->n ; j++)
	  {
	    int k=0;
	    Loc->C[i+Loc->m*j].r = PLUS(A->C[i+A->m*k].r,B->C[k+B->m*j].r);
	    Loc->C[i+Loc->m*j].i = PLUS(A->C[i+A->m*k].i,B->C[k+B->m*j].i);
	    for ( k= 1; k < A->n ; k++) 
	      {
		Loc->C[i+Loc->m*j].r= Max( PLUS(A->C[i+A->m*k].r, B->C[k+B->m*j].r),Loc->C[i+Loc->m*j].r);
		Loc->C[i+Loc->m*j].i= Max( PLUS(A->C[i+A->m*k].i, B->C[k+B->m*j].i),Loc->C[i+Loc->m*j].r);
	      }
	  }
    }
  else 
    {
      int i,j;
      for ( i = 0 ; i < A->m ; i++) 
	for ( j = 0 ; j < B->n ; j++)
	  {
	    int k=0;
	    Loc->R[i+Loc->m*j]= PLUS(A->R[i+A->m*k], B->R[k+B->m*j]);
	    for ( k= 1; k < A->n ; k++) 
	      {
		Loc->R[i+Loc->m*j]= Max(PLUS(A->R[i+A->m*k], B->R[k+B->m*j]),Loc->R[i+Loc->m*j]);
	      }
	  }
    }
  return(Loc);
}




/*
 * Matrix product in min plus algebra 
 * 
 */

NspMatrix *nsp_mat_minplus_mult(NspMatrix *A, NspMatrix *B)
{  
  NspMatrix *Loc;
  if ( A->n != B->m ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(NULLMAT);
    }
  if ( A->rc_type == 'c' ) 
    {
      if ( B->rc_type == 'r' ) 
	{
	  if (nsp_mat_set_ival(B,0.00) == FAIL ) return(NULLMAT);
	}
    }
  else 
    { 
      if ( B->rc_type == 'c' ) 
	{
	  if (nsp_mat_set_ival(A,0.00) == FAIL ) return(NULLMAT);
	}
    }
  if ((Loc = nsp_matrix_create(NVOID,A->rc_type,A->m,B->n))==  NULLMAT) return(NULLMAT);
  if ( Loc->rc_type == 'c' ) 
    {
      int i,j;
      for ( i = 0 ; i < A->m ; i++) 
	for ( j = 0 ; j < B->n ; j++)
	  {
	    int k=0;
	    Loc->C[i+Loc->m*j].r = PLUS( A->C[i+A->m*k].r, B->C[k+B->m*j].r);
	    Loc->C[i+Loc->m*j].i = PLUS(A->C[i+A->m*k].i, B->C[k+B->m*j].i);
	    for ( k= 1; k < A->n ; k++) 
	      {
		Loc->C[i+Loc->m*j].r= Min(PLUS(A->C[i+A->m*k].r, B->C[k+B->m*j].r),Loc->C[i+Loc->m*j].r);
		Loc->C[i+Loc->m*j].i= Min(PLUS(A->C[i+A->m*k].i, B->C[k+B->m*j].i),Loc->C[i+Loc->m*j].r);
	      }
	  }
    }
  else 
    {
      int i,j;
      for ( i = 0 ; i < A->m ; i++) 
	for ( j = 0 ; j < B->n ; j++)
	  {
	    int k=0;
	    Loc->R[i+Loc->m*j]= PLUS(A->R[i+A->m*k], B->R[k+B->m*j]);
	    for ( k= 1; k < A->n ; k++) 
	      {
		Loc->R[i+Loc->m*j]= Min(PLUS(A->R[i+A->m*k], B->R[k+B->m*j]),Loc->R[i+Loc->m*j]);
	      }
	  }
    }
  return(Loc);
}



/*
 * Max plus addition 
 *   A = A+B (covers the scalar and [] cases ) 
 */

int nsp_mat_maxplus_add(NspMatrix *A, NspMatrix *B) 
{
  return  nsp_mat_mult_tt(A,B);
}



/* a set of functions adapted for the MTLB_MODE */
int nsp_mat_add_scalar_bis(NspMatrix *A, NspMatrix *B) 
{
  int i;
  if ( A->mn == 0 )
    return OK;

  if ( A->rc_type == 'r' )
    {
      if ( B->rc_type == 'r' )
	for ( i = 0 ; i < A->mn ; i++ ) 
	  A->R[i] += B->R[0];
      else
	{
	  if ( nsp_mat_complexify(A, B->C[0].i) == FAIL ) return FAIL;
	  for ( i = 0 ; i < A->mn ; i++ ) 
	    A->C[i].r += B->C[0].r;
	}
    }
  else
    {
      if ( B->rc_type == 'r' )
	for ( i = 0 ; i < A->mn ; i++ ) 
	  A->C[i].r += B->R[0];
      else
	for ( i = 0 ; i < A->mn ; i++ ) 
	  { A->C[i].r += B->C[0].r;  A->C[i].i += B->C[0].i; }
    }
  return OK;
}

int nsp_mat_add_mat(NspMatrix *A, NspMatrix *B)
{
  int i;
  if (SameDim(A,B))
    {
      if ( A->rc_type == 'r' ) 
	{
	  if ( B->rc_type == 'r') 
	    for ( i = 0 ; i < A->mn ; i++ ) 
	      A->R[i] += B->R[i];
	  else 
	    {
	      if ( nsp_mat_complexify(A,0.0) == FAIL ) return FAIL;
	      for ( i = 0 ; i < A->mn ; i++ ) 
		{ A->C[i].r += B->C[i].r;  A->C[i].i = B->C[i].i; }
	    }
	}
      else 
	{
	  if ( B->rc_type == 'r') 
	    for ( i = 0 ; i < A->mn ; i++ ) 
	      A->C[i].r += B->C[i].r;
	  else 
	    for ( i = 0 ; i < A->mn ; i++ ) 
	      { A->C[i].r += B->C[i].r;  A->C[i].i += B->C[i].i; }
	}
      return OK;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return FAIL;
    }
}

/* A is a matrix, B is a scalar A - B */
int nsp_mat_sub_scalar_bis(NspMatrix *A, NspMatrix *B) 
{
  int i;
  if ( A->mn == 0 )
    return OK;

  if ( A->rc_type == 'r' )
    {
      if ( B->rc_type == 'r' )
	for ( i = 0 ; i < A->mn ; i++ ) 
	  A->R[i] -= B->R[0];
      else
	{
	  if ( nsp_mat_complexify(A, -B->C[0].i) == FAIL ) return FAIL;
	  for ( i = 0 ; i < A->mn ; i++ ) 
	    A->C[i].r -= B->C[0].r;
	}
    }
  else
    {
      if ( B->rc_type == 'r' )
	for ( i = 0 ; i < A->mn ; i++ ) 
	  A->C[i].r -= B->R[0];
      else
	for ( i = 0 ; i < A->mn ; i++ ) 
	  { A->C[i].r -= B->C[0].r;  A->C[i].i -= B->C[0].i; }
    }
  return OK;
}

/* A is a matrix, B is a scalar A <- B - A */
int nsp_scalar_sub_mat_bis(NspMatrix *A, NspMatrix *B) 
{
  int i;
  if ( A->mn == 0 )
    return OK;

  if ( A->rc_type == 'r' )
    {
      if ( B->rc_type == 'r' )
	for ( i = 0 ; i < A->mn ; i++ ) 
	  A->R[i] =  B->R[0] - A->R[i];
      else
	{
	  if ( nsp_mat_complexify(A, B->C[0].i) == FAIL ) return FAIL;
	  for ( i = 0 ; i < A->mn ; i++ ) 
	    A->C[i].r = B->C[0].r - A->C[i].r;
	}
    }
  else
    {
      if ( B->rc_type == 'r' )
	for ( i = 0 ; i < A->mn ; i++ ) 
	  { A->C[i].r = B->R[0] - A->C[i].r; A->C[i].i = -A->C[i].i; }  
      else
	for ( i = 0 ; i < A->mn ; i++ ) 
	  { A->C[i].r = B->C[0].r - A->C[i].r;  A->C[i].i = B->C[0].i - A->C[i].i; }
    }
  return OK;
}

/* deals with the case dim A == dim B **/

int nsp_mat_sub_mat(NspMatrix *A, NspMatrix *B)
{
  int i;
  if (SameDim(A,B))
    {
      if ( A->rc_type == 'r' ) 
	{
	  if ( B->rc_type == 'r') 
	    for ( i = 0 ; i < A->mn ; i++ ) 
	      A->R[i] -= B->R[i];
	  else 
	    {
	      if ( nsp_mat_complexify(A,0.0) == FAIL ) return FAIL;
	      for ( i = 0 ; i < A->mn ; i++ ) 
		{ A->C[i].r -= B->C[i].r;  A->C[i].i = -B->C[i].i; }
	    }
	}
      else 
	{
	  if ( B->rc_type == 'r') 
	    for ( i = 0 ; i < A->mn ; i++ ) 
	      A->C[i].r -= B->C[i].r;
	  else 
	    for ( i = 0 ; i < A->mn ; i++ ) 
	      { A->C[i].r -= B->C[i].r;  A->C[i].i -= B->C[i].i; }
	}
      return OK;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return FAIL;
    }
}

int nsp_mat_mult_scalar_bis(NspMatrix *A, NspMatrix *B) 
{
  int i;

/*   if ( A->mn == 0 ) */
/*     return OK; */

  if ( A->rc_type == 'r' )
    {
      if ( B->rc_type == 'r' )
	for ( i = 0 ; i < A->mn ; i++ ) 
	  A->R[i] *= B->R[0];
      else
	{
	  if ( nsp_mat_complexify(A, 0.0) == FAIL ) return FAIL;
	  for ( i = 0 ; i < A->mn ; i++ ) 
	    { A->C[i].i = A->C[i].r * B->C[0].i; A->C[i].r *= B->C[0].r; }
	}
    }
  else
    {
      if ( B->rc_type == 'r' )
	for ( i = 0 ; i < A->mn ; i++ ) 
	  { A->C[i].r *= B->R[0]; A->C[i].i *= B->R[0]; }
      else
	for ( i = 0 ; i < A->mn ; i++ ) 
	  nsp_prod_c(&A->C[i],&B->C[i]);
    }
  return OK;
}
