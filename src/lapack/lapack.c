/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2005-2009 Bruno Pincon Esial/Iecn
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
 * a set of linear algebra functions with lapack 
 *
 */

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "nsp/interf.h"
#include "nsp/lapack-c.h"
#include "nsp/cnumeric.h"
#include "nsp/blas.h"
#include "nsp/nsp_lapack.h"
#include "nsp/matint.h"



/**
 * nsp_mat_is_symmetric:
 * @A: a real or complex matrix
 * 
 * test if the matrix @A is symetric (real case) or hermitian (complex case)
 * 
 * Return value: %TRUE or %FALSE
 **/
int nsp_mat_is_symmetric(const NspMatrix *A)
{
  int i,j;
  if ( A->m != A->n ) return FALSE;
  if ( A->rc_type == 'r') 
    {
      for ( i=0 ; i < A->m ; i++)
	for ( j=0 ; j < i ; j++)
	  if ( A->R[i+j*A->m] != A->R[j+i*A->m]) 
	    return FALSE;
    }
  else 
    {
      for ( i=0 ; i < A->m ; i++)
	  for ( j=0 ; j <= i ; j++)
	    if ( A->C[i+j*A->m].r != A->C[j+i*A->m].r  ||  A->C[i+j*A->m].i != -A->C[j+i*A->m].i )
	      return FALSE;
    }
  return TRUE;
}

/**
 * nsp_mat_is_lower_triangular:
 * @A: a real or complex matrix
 * 
 * test if the matrix @A is lower triangular
 * 
 * Return value: %TRUE or %FALSE 
 **/
int nsp_mat_is_lower_triangular(NspMatrix *A)
{
  int i,j;
  if ( A->rc_type == 'r') 
    {
      for ( j=1 ; j < A->n ; j++)
	for ( i=0 ; i < Min(j,A->m) ; i++)
	  if ( A->R[i+j*A->m] != 0.0 ) 
	    return FALSE;
    }
  else 
    {
      for ( j=1 ; j < A->n ; j++)
	for ( i=0 ; i < Min(j,A->m) ; i++)
	  if ( A->C[i+j*A->m].r != 0.0  ||  A->C[i+j*A->m].i != 0.0 ) 
	    return FALSE;
    }
  return TRUE;
}

/**
 * nsp_mat_is_upper_triangular:
 * @A: a real or complex matrix
 * 
 * test if the matrix @A is upper triangular
 * 
 * Return value: %TRUE or %FALSE 
 **/
int nsp_mat_is_upper_triangular(NspMatrix *A)
{
  int i,j;
  if ( A->rc_type == 'r') 
    {
      for ( j=0 ; j < Min(A->m,A->n) ; j++)
	for ( i=j+1 ; i < A->m ; i++)
	  if ( A->R[i+j*A->m] != 0.0 ) 
	    return FALSE;
    }
  else 
    {
      for ( j=0 ; j < Min(A->m,A->n) ; j++)
	for ( i=j+1 ; i < A->m ; i++)
	  if ( A->C[i+j*A->m].r != 0.0  ||  A->C[i+j*A->m].i != 0.0 ) 
	    return FALSE;
    }
  return TRUE;
}

/**
 * nsp_mat_lower_and_upper_bandwidth:
 * @A: (input) a real or complex matrix
 * @Kl: (output) the lower bandwidth
 * @Ku: (output) the upper bandwidth
 *
 * computes the lower and upper bandwith of @A
 * 
 * Return value: %FAIL (if the matrix is not square) or %OK 
 **/
int nsp_mat_lower_and_upper_bandwidth(NspMatrix *A, int *Kl, int *Ku)
{
  int i,j, n=A->m, kl=0, ku=0;

  if ( A->m != A->n )
    return FAIL;

  if ( A->rc_type == 'r') 
    {
      /* lower bandwith */
      for ( j = 0 ; j < n-kl-1 ; j++ )
	for ( i = n-1 ; i > j+kl ; i-- )
	  if ( A->R[i+j*n] != 0.0 ) 
	    kl = i-j;
      /* upper bandwith */
      for ( j = n-1 ; j > ku ; j-- )
	for ( i = 0 ; i < j-ku ; i++ )
	  if ( A->R[i+j*n] != 0.0 ) 
	    ku = j-i;
    }
  else 
    {
      /* lower bandwith */
      for ( j = 0 ; j < n-kl-1 ; j++ )
	for ( i = n-1 ; i > j+kl ; i-- )
	  if ( A->C[i+j*n].r != 0.0  ||  A->C[i+j*n].i != 0.0 ) 
	    kl = i-j;
      /* upper bandwith */
      for ( j = n-1 ; j > ku ; j-- )
	for ( i = 0 ; i < j-ku ; i++ )
	  if ( A->C[i+j*n].r != 0.0  ||  A->C[i+j*n].i != 0.0 ) 
	    ku = j-i;
    }

  *Kl = kl; *Ku = ku;
  return OK;
}

/**
 * nsp_mat_have_nan_or_inf:
 * @A: a real or complex matrix
 * 
 * test if the matrix @A contains Nan or +-Inf
 * 
 * Return value: %TRUE (if at least one element is Nan or +-Inf) or %FALSE 
 **/
int nsp_mat_have_nan_or_inf(NspMatrix *A)
{
  int i;

  if ( A->rc_type == 'r')
    {
      for ( i= 0 ; i < A->mn ; i++ ) 
	if (isinf (A->R[i]) || isnan (A->R[i]))
	  return TRUE;
    }
  else /* A->rc_type == 'i' */
    {
      for ( i= 0 ; i < A->mn ; i++ ) 
	if (nsp_isinf_c (&A->C[i]) || nsp_isnan_c (&A->C[i]))
	  return TRUE;
    }

  return FALSE;
}

/**
 * xerbla:
 * @srname: name of the (lapack) routine which has detected an error
 * @info: 
 * 
 * switch lapack message to nsp message
 * 
 * Return value: 0
 **/

int C2F(xerbla)(char *srname,int * info,int srname_len)
{
  int i;
  char mes[7];
  for (i=0; i < 6 ; i++) mes[i]=srname[i];
  mes[6]='\0';
  Scierror("Error: On entry to %s parameter number %d had an illegal value\n",
	   mes,*info);
  return 0;
}


static int intdgeqrpf(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E,
		      NspMatrix **Rank, NspMatrix **Sval, double *tol,char flag);
static int intzgeqrpf(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E,
		      NspMatrix **Rank, NspMatrix **Sval, double *tol,char flag);

/**
 * nsp_qr:
 * @A: (input) a real or complex matrix of size m x n. @A is not modified.
 * @Q: (output) a real orthogonal or complex unitary matrix of 
 *              size m x m (usual factorization) or m x Min(n,m) (economic factorization)
 * @R: (output) a real or complex upper triangular matrix of size m x n (usual factorization)
 *              or Min(m,n) x n (economic factorization)
 * @E: (output if @E is not NULL on entry) a permutation vector (m x 1 ) 
 *     stored as a real vector
 * @Rank: (output if @Rank is not NULL on entry) integer scalar (1 x 1) stored as 
 *        a real, the estimated rank of @A
 * @Sval: (output if @Rank != NULL and @Sval != NULL on entry) a real vector (3 x 1), 
 *        approximation of some singular values of @A
 * @tol:  (input) the tolerance for the rank estimation, if @tol is NULL on entry
 *        Max(m,n)*eps is used 
 * @mode: (input) char flag which can be 'x' (usual factorization) or 'e' (economic factorization)
 * 
 * Compute a QR factorization of the matrix @A :
 *
 *      @A P = @Q @R   if @E is not NULL
 *
 *      @A = @Q @R   if @E is NULL
 * 
 *      if P is the permutation matrix associated to the permutation vector @E
 *      (AP)(:,j) = A(:,E(j))
 *
 * Return value:  %OK or %FAIL
 **/
int nsp_qr(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E, NspMatrix **Rank, 
	   NspMatrix **Sval, double *tol, char mode)
{
  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  {
    if ( (*Q =nsp_matrix_create(NVOID,A->rc_type,A->m,0)) == NULLMAT ) return FAIL;
    if ( (*R =nsp_matrix_create(NVOID,A->rc_type,0,A->n)) == NULLMAT ) return FAIL;
    if ( E != NULL)
      {
	if ( (*E =nsp_matrix_create(NVOID,'r',A->n,1)) == NULLMAT ) return FAIL;
	int i;
	for ( i = 0 ; i < A->n ; i++ ) (*E)->R[i] = i+1;
      }
    if (Rank != NULL ) 
    {
      if ( (*Rank =nsp_matrix_create(NVOID,'r',0,0)) == NULLMAT ) return FAIL;
    }
    if (Sval != NULL ) 
    {
      if ( (*Sval =nsp_matrix_create(NVOID,'r',0,0)) == NULLMAT ) return FAIL;
    }
    return OK ; 
  }

  if (A->rc_type == 'r' ) 
    return  intdgeqrpf(A,Q,R,E,Rank,Sval,tol,mode);
  else 
    return  intzgeqrpf(A,Q,R,E,Rank,Sval,tol,mode);
}

/*  qr for real NspMatrix A */ 
static int intdgeqrpf(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E,
		      NspMatrix **Rank, NspMatrix **Sval, double *tol_rcond,char flag)
{
  int i, j;
  NspMatrix *q=NULLMAT, *r=NULLMAT, *e=NULLMAT, *rank=NULLMAT, *sval=NULLMAT;
  double *work=NULL, qwork[1], *tau=NULL;
  int lwork, lworkbis, info, *jpvt=NULL, m=A->m, n=A->n, Minmn=Min(m,n);

  if ( (r=nsp_matrix_copy(A)) == NULLMAT ) return FAIL ; 

  if ( (tau=nsp_alloc_work_doubles(Minmn)) == NULL ) goto err;

  lwork = -1;
  lworkbis = -1;

  if ( E == NULL ) 
    {
      /* query optimal size for dgeqrf but also for dorgqr */
      C2F(dgeqrf)(&m, &n, r->R, &m, tau, qwork, &lwork, &info);
      lwork = (int) qwork[0];
      C2F(dorgqr)(&m, (flag=='e') ? (&Minmn):(&m), &Minmn, NULL, &m, NULL, qwork, &lworkbis, &info);
      lworkbis = (int) qwork[0];
      lwork = Max( lwork, lworkbis );
      if ( (work=nsp_alloc_work_doubles(lwork)) == NULL ) goto err;
      C2F(dgeqrf)(&m, &n, r->R, &m, tau, work, &lwork, &info);
    } 
  else 
    {
      if ( (jpvt=nsp_alloc_work_int(n)) == NULL ) goto err;
      for (i = 0; i < n ; i++) jpvt[i]= 0;
      /* query optimal size for dgeqp3 but also for dorgqr */
      C2F(dgeqp3)(&m, &n, r->R, &m, jpvt, tau, qwork, &lwork, &info);
      lwork = (int) qwork[0];
      C2F(dorgqr)(&m, (flag=='e') ? (&Minmn):(&m), &Minmn, NULL, &m, NULL, qwork, &lworkbis, &info);
      lworkbis = (int) qwork[0];
      lwork = Max( lwork, lworkbis );
      if ( (work=nsp_alloc_work_doubles(lwork)) == NULL ) goto err;
      C2F(dgeqp3)(&m, &n, r->R, &m, jpvt, tau, work, &lwork, &info);
      if ( (e=nsp_matrix_create(NVOID,'r', n, 1)) == NULLMAT ) goto err; 
      for ( i = 0 ; i < n ; i++ ) e->R[i] = (double) jpvt[i];
   }

  /* make Q (Q is an mxm matrix or mxMinmn) */ 
  if ( (q=nsp_matrix_create(NVOID,'r',m, (flag=='e') ? Minmn:m)) == NULLMAT ) 
    goto err;
  memcpy(q->R, r->R, m*Minmn*sizeof(double));

  C2F(dorgqr)(&m, (flag=='e') ? (&Minmn):(&m), &Minmn, q->R, &m, tau, work, &lwork, &info);

  /* make R */ 
  nsp_mat_triu(r,0);
  if ( flag == 'e' && m > n )  /* we must delete the last rows of R */ 
    {
      for ( j = 1 ; j < n ; j++ )
	memmove(&(r->R[j*n]), &(r->R[j*m]), n*sizeof(double)); 
      r->m = n; r->mn = n*n;
      r->R = realloc(r->R, r->mn*sizeof(double));
    }

  /* if requested we compute the rank by incremental estimation of min and max */
  /* singular values and vectors of the triangular matrice R (see dgelsy.f) */ 
  if (Rank != NULL ) 
    {
      int IRank, imin=2, imax=1, ic, id;
      double Tol_rcond, smax, smin, tt=Abs(r->R[0]), *vmin=work, *vmax=&work[Minmn];
      double sminpr, smaxpr, s1, c1, s2, c2;

      if (tol_rcond == NULL ) 
	Tol_rcond = Max(m,n)*nsp_dlamch("eps");
      else 
	Tol_rcond = *tol_rcond;

      if ( tt == 0.0 )
	IRank = 0;
      else
	{
	  IRank = 1; vmin[0] = 1.0; vmax[0] = 1.0; 
	  smin = tt; smax = tt; ic = r->m; id = r->m+1;
	  while ( IRank < Minmn )
	    {
	      C2F(dlaic1)(&imin, &IRank, vmin, &smin, &(r->R[ic]), &(r->R[id]), &sminpr, &s1, &c1);
 	      C2F(dlaic1)(&imax, &IRank, vmax, &smax, &(r->R[ic]), &(r->R[id]), &smaxpr, &s2, &c2);
	      if ( smaxpr*Tol_rcond > sminpr ) break;
	      for ( i = 0 ; i < IRank; i++ ) { vmin[i] *= s1; vmax[i] *= s2; }
	      vmin[IRank] = c1; vmax[IRank] = c2;
	      smin = sminpr; smax = smaxpr;
	      IRank++; ic += r->m; id += r->m+1;
	    }
	}

      if ( (rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) goto err;
      rank->R[0]=IRank;

      if ( Sval != NULL )
	{
	  if ( (sval=nsp_matrix_create(NVOID,'r',3,1)) == NULLMAT) goto err;
	  if ( IRank == 0 ) 
	    { sval->R[0] = sval->R[1] = sval->R[2] = 0.0; }
	  else 
	    {
	      sval->R[0] = smax; sval->R[1] = smin;
	      if ( IRank < Minmn ) sval->R[2] = sminpr; else sval->R[2] = smin;
	    }
	}
    }
  
  FREE(tau); FREE(work);
  if ( E != NULL) {FREE(jpvt); *E = e;}
  *R = r; *Q = q;
  if ( Rank != NULL ) *Rank = rank;
  if ( Sval != NULL ) *Sval = sval;
  return OK;

 err:
  FREE(tau); FREE(work); FREE(jpvt);
  nsp_matrix_destroy(q); nsp_matrix_destroy(r);
  nsp_matrix_destroy(e); nsp_matrix_destroy(rank);
  return FAIL;
}


/*  qr for complex NspMatrix A */ 
static int intzgeqrpf(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E,
		      NspMatrix **Rank, NspMatrix **Sval, double *tol_rcond,char flag)
{
  int i, j;
  NspMatrix *q=NULLMAT, *r=NULLMAT, *e=NULLMAT, *rank=NULLMAT, *sval=NULLMAT;
  double *rwork=NULL;
  doubleC *cwork=NULL, qwork[1], *tau=NULL;
  int lwork, lworkbis, info, *jpvt=NULL, m=A->m, n=A->n, Minmn=Min(m,n);

  if ( (r=nsp_matrix_copy(A)) == NULLMAT ) return FAIL ; 

  if ( (tau=nsp_alloc_work_doubleC(Minmn)) == NULL ) goto err;

  lwork = -1;
  lworkbis = -1;

  if ( E == NULL ) 
    {
      /* query optimal work size for zgeqrf but also for zungqr */
      C2F(zgeqrf)(&m, &n, r->C, &m, tau, qwork, &lwork, &info);
      lwork = (int) qwork[0].r;
      C2F(zungqr)(&m, (flag=='e') ? (&Minmn):(&m), &Minmn, NULL, &m, NULL, qwork, &lworkbis, &info);
      lworkbis = (int) qwork[0].r;
      lwork = Max ( lwork, lworkbis );
      if ( (cwork=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;
      C2F(zgeqrf)(&m, &n, r->C, &m, tau, cwork, &lwork, &info);
    } 
  else 
    {
      if ( (rwork=nsp_alloc_work_doubles(2*n)) == NULL ) goto err;
      if ( (jpvt=nsp_alloc_work_int(n)) == NULL ) goto err;
      for (i = 0; i < n ; i++) jpvt[i]= 0;
      /* query optimal work size for zgeqp3 but also for zungqr */
      C2F(zgeqp3)(&m, &n, r->C, &m, jpvt, tau, qwork, &lwork, rwork, &info);
      lwork = (int) qwork[0].r;
      C2F(zungqr)(&m, (flag=='e') ? (&Minmn):(&m), &Minmn, NULL, &m, NULL, qwork, &lworkbis, &info);
      lworkbis = (int) qwork[0].r;
      lwork = Max ( lwork, lworkbis );
      if ( (cwork=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;
      C2F(zgeqp3)(&m, &n, r->C, &m, jpvt, tau, cwork, &lwork, rwork, &info);
      if ( (e=nsp_matrix_create(NVOID,'r', n, 1)) == NULLMAT ) goto err; 
      for ( i = 0 ; i < n ; i++ ) e->R[i] = (double) jpvt[i];
   }

  /* make Q (Q is an mxm matrix or mxMinmn) */ 
  if ( (q=nsp_matrix_create(NVOID,'c',m, (flag=='e') ? Minmn:m)) == NULLMAT ) 
    goto err;
  memcpy(q->C, r->C, m*Minmn*sizeof(doubleC));
  C2F(zungqr)(&m, (flag=='e') ? (&Minmn):(&m), &Minmn, q->C, &m, tau, cwork, &lwork, &info);

  /* make R */ 
  nsp_mat_triu(r,0); 
  if ( flag == 'e' && m > n )  /* we must delete the last rows of R */ 
    {
      for ( j = 1 ; j < n ; j++ )
	memmove(&(r->C[j*n]), &(r->C[j*m]), n*sizeof(doubleC)); 
      r->m = n; r->mn = n*n;
      r->C = realloc(r->C, r->mn*sizeof(doubleC));
    }

  /* if requested we compute the rank by incremental estimation of min and max */
  /* singular values and vectors of the triangular matrice R (see zgelsy.f) */ 
  if ( Rank != NULL ) 
    {
      int IRank, imin=2, imax=1, ic, id;
      double Tol_rcond, smax, smin, tt=nsp_abs_c(&(r->C[0]));
      doubleC *vmin=cwork, *vmax=&cwork[Minmn], cone={1.0,0.0}, s1, c1, s2, c2;
      double sminpr, smaxpr;
      if (tol_rcond == NULL ) 
	Tol_rcond = Max(m,n)*nsp_dlamch("eps");
      else 
	Tol_rcond = *tol_rcond;

      if ( tt == 0.0 )
	IRank = 0;
      else
	{
	  IRank = 1; vmin[0] = cone; vmax[0] = cone; 
	  smin = tt; smax = tt; ic = r->m; id = r->m+1;
	  while ( IRank < Minmn )
	    {
	      C2F(zlaic1)(&imin, &IRank, vmin, &smin, &(r->C[ic]), &(r->C[id]), &sminpr, &s1, &c1);
 	      C2F(zlaic1)(&imax, &IRank, vmax, &smax, &(r->C[ic]), &(r->C[id]), &smaxpr, &s2, &c2);
	      if ( smaxpr*Tol_rcond > sminpr ) break;
	      for ( i = 0 ; i < IRank; i++ ) { nsp_prod_c(&(vmin[i]),&s1); nsp_prod_c(&(vmax[i]),&s2);}
	      vmin[IRank] = c1; vmax[IRank] = c2;
	      smin = sminpr; smax = smaxpr;
	      IRank++; ic += r->m; id += r->m+1;
	    }
	}
      if ( (rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) goto err;
      rank->R[0]=IRank;

      if ( Sval != NULL )
	{
	  if ( (sval=nsp_matrix_create(NVOID,'r',3,1)) == NULLMAT) goto err;
	  if ( IRank == 0 ) 
	    { sval->R[0] = sval->R[1] = sval->R[2] = 0.0; }
	  else 
	    {
	      sval->R[0] = smax; sval->R[1] = smin;
	      if ( IRank < Minmn ) sval->R[2] = sminpr; else sval->R[2] = smin;
	    }
	}
    }
  
  FREE(tau); FREE(cwork);
  if ( E != NULL) {FREE(rwork); FREE(jpvt); *E = e;}
  *R = r; *Q = q;
  if ( Rank != NULL ) *Rank = rank;
  if ( Sval != NULL ) *Sval = sval;
  return OK;

 err:
  FREE(tau); FREE(cwork); FREE(rwork); FREE(jpvt);
  nsp_matrix_destroy(q); nsp_matrix_destroy(r);
  nsp_matrix_destroy(e); nsp_matrix_destroy(rank);
  return FAIL;
}


int intdgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **E,NspMatrix **Rcond);
int intzgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **E,NspMatrix **Rcond);

/**
 * nsp_lu:
 * @A: (input/output) a real or complex matrix of size m x n on input
 *                    and a real or complex upper triangular matrix of size Min(m,n) x n on output
 * @L: (output) a real or complex lower triangular matrix of size m x Min(m,n). If @E is NULL
 *              on entry then @L stores inv(P) L and is no more lower triangular.
 * @E: (output if @E is not NULL on entry) a permutation vector (m x 1 ) 
 *             stored as a real vector
 * @Rcond: (output if @Rcond is not NULL on entry) a scalar (estimation of the reciprocal
 *             condition number in 1-norm.
 * 
 * Compute a LU factorization of the matrix @A :
 *
 *     P A = L U
 *
 *  @A is changed and stores the U factor on output.
 *
 *  the permutation @E is returned as a permutation vector
 *  (precisely E is the inverse permutation of the one 
 *   associated to permutation matrix P in PA = LU).
 * 
 * Return value:  %OK or %FAIL
 **/
int nsp_lu(NspMatrix *A,NspMatrix **L,NspMatrix **E, NspMatrix **Rcond)
{

  if ( A->m != A->n  &&  Rcond != NULL )
    {
      Scierror("Error: rcond argument should be a square matrix\n"); 
      return FAIL;
    }

  /* special return when A is an empty matrice */
  int i;
  if ( A->mn == 0 )
    {
      if ( (*L=nsp_matrix_create(NVOID,A->rc_type,A->m,0)) == NULLMAT ) return FAIL;
      if ( E != NULL )
	{
	  if ( (*E=nsp_matrix_create(NVOID,'r',A->m,1)) == NULLMAT ) return FAIL;
	  for ( i = 0; i < A->m ; i++ ) (*E)->R[i] = i+1;
	}
      if ( Rcond != NULL )
	{
	  if ( (*Rcond=nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) return FAIL;
	  (*Rcond)->R[0] = 1.0/0.0;
	}

      A->m = 0;  /* because A store U... */
      return OK ;
    }

  if (A->rc_type == 'r' ) 
    return  intdgetrf(A,L,E,Rcond);
  else 
    return  intzgetrf(A,L,E,Rcond);
}

int intdgetrf(NspMatrix *A, NspMatrix **pL, NspMatrix **pE, NspMatrix **pRcond)
{
  int info, m=A->m, n=A->n , Minmn, i, j, k, ind, nbsd;
  int *ipiv=NULL, *invp=NULL, *iwork=NULL;
  double zeroD=0.0, oneD=1.0, rcond, anorm, *dwork=NULL;
  NspMatrix *L=NULLMAT, *E=NULLMAT, *Rcond=NULLMAT;

  Minmn = Min(m,n);

  if ( (L=nsp_matrix_create(NVOID,A->rc_type,m,Minmn)) == NULLMAT ) return FAIL;
 
  if ( pE != NULL )
    if ( (E=nsp_matrix_create(NVOID,'r',m,1)) == NULLMAT ) goto err;
  
  if ( pRcond != NULL )
    {
      if ( (Rcond=nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) goto err;
      if ( (iwork = nsp_alloc_work_int(n)) == NULL ) goto err;
      if ( (dwork =nsp_alloc_work_doubles(4*n)) == NULL ) goto err;
    }

  if ( (ipiv = nsp_alloc_work_int(Minmn)) == NULL ) goto err;
  if ( (invp = nsp_alloc_work_int(m)) == NULL ) goto err;
  
  if ( pRcond != NULL )
    anorm = C2F(dlange)("1", &n, &n, A->R, &n, NULL, 1L);

  C2F(dgetrf)(&m, &n, A->R, &m, ipiv, &info);

  if ( pRcond != NULL )   /* may be not NULL only if m == n */
    {
      if (info == 0) 
	C2F(dgecon)("1", &n, A->R, &n, &anorm, &rcond, dwork, iwork, &info, 1L);
      else if (info > 0)   /* there is a zero pivot */
	rcond = 0.0;
      Rcond->R[0] = rcond;
    }
  
  /* compute the inverse permutation invpiv */ 
  for ( i=1 ; i <= m ; i++) invp[i-1]=i;
  for ( i=1 ; i <= Minmn ; i++) 
    {
      int ip = ipiv[i-1];
      if ( ip != i) { int iw = invp[i-1]; invp[i-1] = invp[ip-1]; invp[ip-1] = iw;}
    }

  /* fill L with the lower triangular part of A */
  if ( pE != NULL )
    {
      for ( i = 0 ; i < m ; i++ ) E->R[i] = (double) invp[i];
      C2F(dlaset)("A", &m, &Minmn, &zeroD, &oneD, L->R, &m, 1L);
      for ( j = 0 ; j < Minmn ; j++ )
	{
	  ind = (m+1)*j+1; nbsd = m-j-1;
	  if ( nbsd > 0 )
	    memcpy(&(L->R[ind]), &(A->R[ind]), nbsd*sizeof(double));
	  for ( k = 0 ; k < nbsd ; k++ ) A->R[ind+k] = 0.0; 	   
	} 
    }
  else  /* but if the permutation is not required we must return P^(-1) L  */
    {
      for ( i = 0 ; i < Minmn ; i++ )
	{
	  k = invp[i] - 1;  /* caution invp is 1-based */ 
	  for ( j = 0 ; j < i ; j++ )
	    { L->R[k+j*m] = A->R[i+j*m];  A->R[i+j*m] = 0.0;}
	  L->R[k+i*m] = 1.0;
	  for ( j = i+1 ; j < Minmn ; j++ )
	    L->R[k+j*m] = 0.0;
	}
      for ( i = Minmn ; i < m ; i++ )
	{
	  k = invp[i] - 1;  /* caution invp is 1-based */ 
	  for ( j = 0 ; j < Minmn ; j++ )
	    { L->R[k+j*m] = A->R[i+j*m];  A->R[i+j*m] = 0.0;}
	}
    }

  if ( m > n )   /* finally compress A to give U */
    {
      for ( j = 1 ; j < n ; j++ )
	memmove(&(A->R[j*n]), &(A->R[j*m]), n*sizeof(double)); 
      A->m = n; A->mn = n*n;
      A->R = realloc(A->R, A->mn*sizeof(double));
    }

  FREE(ipiv); FREE(invp);
  if ( pRcond != NULL ) { FREE(iwork); FREE(dwork);*pRcond = Rcond; }
  if ( pE != NULL ) *pE = E;
  *pL = L;
  return OK;

 err:
  FREE(ipiv); FREE(invp); FREE(iwork); FREE(dwork);
  nsp_matrix_destroy(L); nsp_matrix_destroy(E); nsp_matrix_destroy(Rcond);   
  return FAIL;
}

int intzgetrf(NspMatrix *A,NspMatrix **pL,NspMatrix **pE, NspMatrix **pRcond)
{
  int info, m=A->m, n=A->n , Minmn, i, j, k, ind, nbsd;
  int *ipiv=NULL, *invp=NULL;
  NspMatrix *L=NULLMAT, *E=NULLMAT, *Rcond=NULLMAT;
  doubleC zeroC={0.0,0.0}, oneC={1.0,0.0}, *cwork=NULL;
  double anorm, rcond, *rwork=NULL;

  Minmn = Min(m,n);

  if ( (L=nsp_matrix_create(NVOID,A->rc_type,m,Minmn)) == NULLMAT ) return FAIL;
 
  if ( pE != NULL )
    if ( (E=nsp_matrix_create(NVOID,'r',m,1)) == NULLMAT ) goto err;
  if ( pRcond != NULL )
    {
      if ( (Rcond=nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) goto err;
      if ( (rwork=nsp_alloc_work_doubles(2*n)) == NULL ) goto err;
      if ( (cwork=nsp_alloc_work_doubleC(2*n)) == NULL ) goto err;
    }

  if ( (ipiv = nsp_alloc_work_int(Minmn)) == NULL ) goto err;
  if ( (invp = nsp_alloc_work_int(m)) == NULL ) goto err;

  if ( pRcond != NULL )
    anorm = C2F(zlange)("1", &n, &n, A->C, &n, NULL, 1L);

  C2F(zgetrf)(&m, &n, A->C, &m, ipiv, &info);

  if ( pRcond != NULL )
    {
      if (info == 0) 
	C2F(zgecon)("1", &n, A->C, &n, &anorm, &rcond, cwork, rwork, &info, 1L);
      else if (info > 0)   /* there is a zero pivot */
	rcond = 0.0;
      Rcond->R[0] = rcond;
    }

  /* compute the inverse permutation invpiv */ 
  for ( i=1 ; i <= m ; i++) invp[i-1]=i;
  for ( i=1 ; i <= Minmn ; i++) 
    {
      int ip = ipiv[i-1];
      if ( ip != i) { int iw = invp[i-1]; invp[i-1] = invp[ip-1]; invp[ip-1] = iw;}
    }

  /* fill L with the inf triangular part of A */
  if ( pE != NULL )
    {
      for ( i = 0 ; i < m ; i++ ) E->R[i] = (double) invp[i];
      C2F(zlaset)("A", &m, &Minmn, &zeroC, &oneC, L->C, &m, 1L);
      for ( j = 0 ; j < Minmn ; j++ )
	{
	  ind = (m+1)*j+1; nbsd = m-j-1;
	  if ( nbsd > 0 )
	    memcpy(&(L->C[ind]), &(A->C[ind]), nbsd*sizeof(doubleC));
	  for ( k = 0 ; k < nbsd ; k++ ) A->C[ind+k] = zeroC; 	   
	} 
    }
  else  /* but if the permutation is not required we must return P^(-1) L  */
    {
      for ( i = 0 ; i < Minmn ; i++ )
	{
	  k = invp[i] - 1;  /* invp is 1-based */ 
	  for ( j = 0 ; j < i ; j++ )
	    { L->C[k+j*m] = A->C[i+j*m];  A->C[i+j*m] = zeroC;}
	  L->C[k+i*m] = oneC;
	  for ( j = i+1 ; j < Minmn ; j++ )
	    L->C[k+j*m] = zeroC;
	}
      for ( i = Minmn ; i < m ; i++ )
	{
	  k = invp[i] - 1;  /* invp is 1-based */ 
	  for ( j = 0 ; j < Minmn ; j++ )
	    { L->C[k+j*m] = A->C[i+j*m];  A->C[i+j*m] = zeroC;}
	}
     }

  if ( m > n )   /* finally compress A to give U */
    {
      for ( j = 1 ; j < n ; j++ )
	memmove(&(A->C[j*n]), &(A->C[j*m]), n*sizeof(doubleC)); 
      A->m = n; A->mn = n*n;
      A->C = realloc(A->C, A->mn*sizeof(doubleC));
    }

  FREE(ipiv); FREE(invp);
  if ( pRcond != NULL ) { FREE(rwork); FREE(cwork);*pRcond = Rcond; }
  if ( pE != NULL ) *pE = E;
  *pL = L;
  return OK;

 err:
  FREE(ipiv); FREE(invp); FREE(rwork); FREE(cwork);
  nsp_matrix_destroy(L); nsp_matrix_destroy(E); nsp_matrix_destroy(Rcond);   
  return FAIL;
}


static int intdgesdd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol);
static int intzgesdd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol);

/**
 * nsp_svd:
 * @A: (input) a real or complex matrix of size m x n (@A is modified).
 * @S: (output) a real vector of size Min(m,n) x 1, the singular values of the matrix @A
 *     in decreasing order.
 * @U: (output if @U is not NULL on entry) a real orthogonal or complex unitary matrix of 
 *     size  m x m (usual factorization) or m x Min(m,n) (economic factorization)  
 * @V: (output if @U is not NULL on entry) a real orthogonal or complex unitary matrix of 
 *     size  n x n (usual factorization) or n x Min(m,n) (economic factorization)  
 * @flag: (input) char flag which can be 'x' (usual factorization) or 'e' (economic factorization)
 * @Rank: (output if @Rank is not NULL on entry) integer scalar (1 x 1) stored as 
 *        a real, the estimated rank of @A
 * @tol:  (input) the tolerance for the rank estimation, if @tol is NULL on entry
 *        Max(m,n)*eps is used 
 *
 * Compute a singular value factorization of the matrix @A :
 *
 *      A = U S V'
 *
 * For rank estimation, the cutting is done for singular values less than tol S_max
 * where S_max is the biggest singular value. 
 *
 * Return value: %OK or %FAIL
 **/
int nsp_svd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol)
{
  /*  A = [] return empty matrices */ 
  if ( A->mn == 0 ) 
    {
      if ( U != NULL)
	{
	  if ( (*U=nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT ) return FAIL;
	  if ( (*V=nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT ) return FAIL;
	}
      if ( (*S=nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT ) return FAIL;
      if ( Rank != NULL )  /* a voir : pourquoi ne pas renvoyer une [] */
	{
	  if ( (*Rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) return FAIL;
	  (*Rank)->R[0] = 0.0;
	}
      return OK ; 
    }

  if ( nsp_mat_have_nan_or_inf(A) )
    {
      Scierror("Error: nan or inf in svd first argument\n"); 
      return FAIL;
    }

  if ( A->rc_type == 'r' ) 
    {
      if ( intdgesdd(A,S,U,V,flag,Rank,tol) == FAIL) return FAIL;
    } 
  else
    {
      if ( intzgesdd(A,S,U,V,flag,Rank,tol) == FAIL) return FAIL;
    }
  return OK;
}

static int intdgesdd(NspMatrix *A, NspMatrix **S, NspMatrix **U, NspMatrix **V, char flag,
		     NspMatrix **Rank, double *tol)
{
  int m = A->m, n=A->n, lwork, info, Minmn = Min(m,n), *iwork=NULL;
  NspMatrix *u=NULLMAT, *s=NULLMAT, *vt=NULLMAT, *v=NULLMAT;
  double *dwork=NULL, qwork[1]; 

  if ( (s=nsp_matrix_create(NVOID,'r',Minmn,1)) == NULLMAT ) return FAIL;

  if ( (iwork=nsp_alloc_work_int(8*Minmn)) == NULL ) goto err;
  lwork = -1;
  if ( U == NULL ) /* just compute the singular values */  
    {
      int lwork1;
      C2F(dgesdd)("N", &m, &n, A->R, &m, s->R, NULL, &m, NULL, &n, qwork, &lwork, iwork, &info, 1L);
      lwork = (int) qwork[0];
      /* the optimal value seams incorrect for large matrices 
       * with liblapck or lapack rmp
       * thus I correct it with the minimum requested.
       */
      lwork1 = 3*Minmn*Minmn + Max(Max(m,n),4*Minmn*Minmn+4*Minmn);
      lwork = Max(lwork, lwork1);
      if ( (dwork=nsp_alloc_work_doubles(lwork)) == NULL ) goto err;
      C2F(dgesdd)("N", &m, &n, A->R, &m, s->R, NULL, &m, NULL, &n, dwork, &lwork, iwork, &info, 1L);
    } 
  else             /* we also compute U and Vt */ 
    {
      int lwork1;
      int nU  = ( flag == 'e') ? Minmn : m;
      int mVt = ( flag == 'e') ? Minmn : n;
      if ( (u=nsp_matrix_create(NVOID,'r',m,nU)) == NULLMAT ) goto err;
      if ( (vt=nsp_matrix_create(NVOID,'r',mVt,n)) == NULLMAT ) goto err;
      C2F(dgesdd)(flag=='e'?"S":"A", &m, &n, A->R, &m, s->R, u->R, &m, vt->R, &mVt, qwork, &lwork, iwork, &info, 1L);
      if (info != 0) 
	{
	  if (info > 0) Scierror("Error: convergence problem in svd\n");
	  goto err;
	} 
      lwork = (int) qwork[0];
      /* the optimal value seams incorrect for large matrices 
       * with liblapck or lapack rmp
       * thus I correct it with the minimum requested.
       */
      lwork1 = 3*Minmn*Minmn + Max(Max(m,n),4*Minmn*Minmn+4*Minmn);
      lwork = Max(lwork, lwork1);
      if ( (dwork=nsp_alloc_work_doubles(lwork)) == NULL ) goto err;
      C2F(dgesdd)(flag=='e'?"S":"A", &m, &n, A->R, &m, s->R, u->R, &m, vt->R, &mVt, dwork, &lwork, iwork, &info, 1L);
      /* build V from its transpose matrix Vt */ 
      if ( (v=nsp_matrix_transpose(vt)) == NULLMAT ) goto err;
    }

  if (info != 0) 
    {
      if (info > 0) Scierror("Error: convergence problem in svd\n");
      goto err;   /* message for info < 0 is given by xerbla.c but this doesn't happen no ? */
    } 

  /* compute the rank if requested */ 
  if ( Rank != NULL ) 
    {
      int i;
      double eps = nsp_dlamch("eps");
      double Tol = ( tol == NULL) ? Max(m,n) * eps * s->R[0] : *tol ; 
      int irank =0 ; 
      for (i = 0 ; i < Minmn; ++i)
	if ( s->R[i] > Tol ) irank = i+1;
	else  break;
      
      if ( (*Rank=nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) goto err;
      (*Rank)->R[0] = (double) irank ; 
    }
  
  FREE(iwork); FREE(dwork);
  *S = s;
  if ( U != NULL) { nsp_matrix_destroy(vt) ; *V = v; *U = u; } 
  return OK;

 err:
  FREE(iwork); FREE(dwork);  
  nsp_matrix_destroy(v); 
  nsp_matrix_destroy(vt);
  nsp_matrix_destroy(u);
  nsp_matrix_destroy(s);
  return FAIL;
} 

static int intzgesdd(NspMatrix *A, NspMatrix **S, NspMatrix **U, NspMatrix **V, char flag,
		     NspMatrix **Rank, double *tol)
{
  int m = A->m, n=A->n, lwork, info, Minmn = Min(m,n), *iwork=NULL;
  NspMatrix *u=NULLMAT, *s=NULLMAT, *vt=NULLMAT, *v=NULLMAT;
  doubleC *cwork=NULL, qwork[1]; 
  double *rwork=NULL;

  if ( (s=nsp_matrix_create(NVOID,'r',Minmn,1)) == NULLMAT ) return FAIL;

  if ( (iwork=nsp_alloc_work_int(8*Minmn)) == NULL ) goto err;
  lwork = -1;
  if ( U == NULL ) /* just compute the singular values */  
    {
      if ( (rwork=nsp_alloc_work_doubles(7*Minmn)) == NULL ) goto err;
      C2F(zgesdd)("N", &m, &n, A->C, &m, s->R, NULL, &m, NULL, &n, qwork, &lwork, rwork, iwork, &info, 1L);
      lwork = (int) qwork[0].r;
      if ( (cwork=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;
      C2F(zgesdd)("N", &m, &n, A->C, &m, s->R, NULL, &m, NULL, &n, cwork, &lwork, rwork, iwork, &info, 1L);
    } 
  else             /* we also compute U and Vt */ 
    {
      int lwork1,lrwork;
      int nU  = ( flag == 'e') ? Minmn : m;
      int mVt = ( flag == 'e') ? Minmn : n;
      if ( (u=nsp_matrix_create(NVOID,'c',m,nU)) == NULLMAT ) goto err;
      if ( (vt=nsp_matrix_create(NVOID,'c',mVt,n)) == NULLMAT ) goto err;
      /* The rwork given in zgelsdd seams wrong !!! */
      lrwork = 5*Minmn*Minmn+5*Minmn;
      lrwork = Max(Max(lrwork,3*m*m),3*n*n);
      if ( (rwork=nsp_alloc_work_doubles(lrwork)) == NULL ) goto err;
      C2F(zgesdd)(flag=='e'?"S":"A", &m, &n, A->C, &m, s->R, u->C, &m, vt->C, &mVt, qwork, &lwork, rwork, iwork, &info, 1L);
      lwork = (int) qwork[0].r;
      lwork1 = Minmn*Minmn + 2*Minmn+Max(m,n);
      lwork = Max(lwork1,lwork);
      if ( (cwork=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;
      C2F(zgesdd)(flag=='e'?"S":"A", &m, &n, A->C, &m, s->R, u->C, &m, vt->C, &mVt, cwork, &lwork, rwork, iwork, &info, 1L);
      /* build V from its transpose matrix Vt */ 
      if ( (v=nsp_matrix_transpose(vt)) == NULLMAT ) goto err;
    }

  if (info != 0) 
    {
      if (info > 0) Scierror("Error: convergence problem in svd\n");
      goto err;   /* message for info < 0 is given by xerbla.c (but this must not happen normally) */
    } 

  /* compute the rank if requested */ 
  if ( Rank != NULL ) 
    {
      int i;
      double eps = nsp_dlamch("eps");
      double Tol = ( tol == NULL) ? Max(m,n) * eps * s->R[0] : *tol ; 
      int irank =0 ; 
      for (i = 0 ; i < Minmn; ++i)
	if ( s->R[i] > Tol ) irank = i+1;
	else  break;
      
      if ( (*Rank=nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) goto err;
      (*Rank)->R[0] = (double) irank ; 
    }
  
  FREE(cwork);
  FREE(rwork); 
  FREE(iwork); 
  *S = s;
  if ( U != NULL) { nsp_matrix_destroy(vt) ; *V = v; *U = u; } 
  return OK;

 err:
  FREE(iwork); FREE(rwork); FREE(cwork); 
  nsp_matrix_destroy(v); 
  nsp_matrix_destroy(vt);
  nsp_matrix_destroy(u);
  nsp_matrix_destroy(s);
  return FAIL;
} 



static int intdgeev(NspMatrix *A,NspMatrix **d,NspMatrix **v); 
static int intzgeev(NspMatrix *A,NspMatrix **d,NspMatrix **v);

/**
 * nsp_spec:
 * @A: (input) a square real or complex matrix : caution @A is modified.
 * @d: (output) a vector with the eigenvalues of the matrix @A
 * @v: (output if not NULL on entry) a square matrix with the
 *     associated eigenvectors : the i th column of @v is the eigenvector
 *     associated to the i th eigenvalues in @d
 * 
 * Computes eigenvalues and (if @v is not NULL on entry) the eigenvectors
 * of the square matrix @A. For symetric real or complex hermitian matrices
 * it is better to use nsp_spec_sym.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_spec(NspMatrix *A, NspMatrix **d,NspMatrix **v) 
{
  /*  A = [] return empty matrices */ 
  int m = A->m,n=A->n;

  if (m != n) 
    { 
      Scierror("Error: first argument for spec should be square and it is (%dx%d)\n", 
	       m,n);
      return FAIL;
    }

  if ( A->mn == 0 ) 
    {
      if (( *d =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      if ( v != NULL) 
	{
	  if (( *v =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) 
	    {
	      nsp_matrix_destroy(*d);
	      return FAIL;
	    }
	}
      return OK ; 
    }
  

  if ( nsp_mat_have_nan_or_inf(A) )
    {
      Scierror("Error: nan or inf in spec first argument\n"); 
      return FAIL;
    }

  if ( A->rc_type == 'r' ) 
    return  intdgeev(A,d,v); 
  else 
    return  intzgeev(A,d,v); 
}

static int intdgeev(NspMatrix *A, NspMatrix **D, NspMatrix **V) 
{
  char type = 'r';
  int n=A->n;
  int info, lwork,i,j;
  double *dwork=NULL,*wr=NULL,*wi=NULL;
  NspMatrix *vr=NULLMAT, *d=NULLMAT, *v=NULLMAT;
  double qwork[1];

  wr = nsp_alloc_work_doubles(n);
  wi = nsp_alloc_work_doubles(n);
  if ( wr == NULL || wi == NULL ) goto err;

  if ( V != NULL )
    if ( (vr=nsp_matrix_create(NVOID,'r',n,n)) == NULLMAT ) goto err;

  lwork = -1;
  C2F(dgeev)("N",(V==NULL)?"N":"V", &n, A->R, &n, wr, wi, NULL, &n, (V==NULL)?NULL:vr->R, 
	     &n, qwork, &lwork, &info, 1L, 1L);
  lwork = (int) qwork[0];

  if (info != 0) 
    {
      Scierror("Error: bad query size in dgeev\n"); 
      goto err;
    }

  if ( (dwork=nsp_alloc_work_doubles(lwork)) == NULL ) goto err;

  C2F(dgeev)("N",(V==NULL)?"N":"V", &n, A->R, &n, wr, wi, NULL, &n, (V==NULL)?NULL:vr->R, 
	     &n, dwork, &lwork, &info, 1L, 1L);

  if (info != 0) 
    {
      Scierror("Error: convergence problem in dgeev\n"); 
      goto err;
    }


  /* result is real ? or complex */ 

  for (i = 0 ; i < n ; ++i)
    if (wi[i] != 0.0) { type = 'c'; break;};

  /* extract results */ 

  /* the spec is copied into d */ 
  if ( (d=nsp_matrix_create(NVOID,type,n,1)) == NULLMAT ) goto err;
  if ( type == 'r' )
    for (i = 0 ; i < n ; ++i) { d->R[i] = wr[i]; }
  else 
    for (i = 0 ; i < n ; ++i) { d->C[i].r = wr[i]; d->C[i].i = wi[i]; }


  if ( V != NULL ) 
    {
      if ( type == 'r' )
	*V = vr;
      else   /* store right (complex) eigenvectors in v */ 
	{
	  if ( (v =nsp_matrix_create(NVOID,type,n,n)) == NULLMAT ) goto err;
	  j = -1;
	  while (1) 
	    {
	      j ++ ; 
	      if (j >= n) break; 
	      if( wi[j] == 0.0 )  /* real eigenvalue => v(j) = vr(j) */
		for ( i = 0;  i < n ; ++i) 
		  { 
		    int k = i+j*n ;
		    v->C[k].r = vr->R[k]; 
		    v->C[k].i = 0.0;
		  }
	      else  /* complex eigenvalue => conjugate pair of eigenvectors: */
                    /*        v(j) = vr(j) + i vr(j+1)   */
                    /*      v(j+1) = vr(j) - i vr(j+1)   */
		{
		  for ( i = 0;  i < n ; ++i) 
		    { 
		      int k = i+j*n ;
		      v->C[k].r = vr->R[k]; 
		      v->C[k].i = vr->R[k+n]; 
		      v->C[k+n].r = vr->R[k]; 
		      v->C[k+n].i = -vr->R[k+n]; 
		    }
		  j++; 
		}
	    }
	  nsp_matrix_destroy(vr);
	  *V = v;
	}
    }
  *D = d;
  FREE(wr); FREE(wi); FREE(dwork);
  return OK;

 err:
  FREE(wr); FREE(wi); FREE(dwork);  
  nsp_matrix_destroy(vr);
  nsp_matrix_destroy(v);
  nsp_matrix_destroy(d);
  return FAIL;
}


static int intzgeev(NspMatrix *A,NspMatrix **D,NspMatrix **V) 
{
  char type = 'r';
  int n=A->n;
  int info, lwork,i;
  NspMatrix *d=NULLMAT, *v=NULLMAT;
  double *rwork=NULL;
  doubleC  qcwork[1], *cwork=NULL;

  if ( (d=nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT ) return FAIL;

  if ( (rwork=nsp_alloc_work_doubles(2*n)) == NULL ) goto err;

  if (V != NULL ) 
    if ( (v=nsp_matrix_create(NVOID,'c',n,n)) == NULLMAT ) goto err;;

  lwork = -1;
  C2F(zgeev)("N", (V==NULL)?"N":"V", &n, A->C, &n, d->C, NULL, &n, (V==NULL)?NULL:v->C, 
	     &n, qcwork, &lwork, rwork, &info, 1L, 1L);
  lwork = (int) qcwork[0].r;
  if ( (cwork=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;
  C2F(zgeev)("N", (V==NULL)?"N":"V", &n, A->C, &n, d->C, NULL, &n, (V==NULL)?NULL:v->C, 
	     &n, cwork, &lwork, rwork, &info, 1L, 1L);
  
  if (info != 0) 
    {
      Scierror("Error: convergence problem in zgeev\n"); 
      goto err;
    }

  /* extract results */ 
  /* result is real ? or complex */ 
  for (i = 0 ; i < n ; ++i) { if (d->C[i].i != 0.0) { type = 'c'; break;}}
  if ( type == 'r' )    /* FIXME: est-ce utile ? */
    if (nsp_mat_get_real(d) == FAIL ) goto err; 

  *D = d; 
  if ( V != NULL ) *V = v;
      
  FREE(rwork); FREE(cwork);
  return OK;

 err:
  FREE(rwork); FREE(cwork);
  nsp_matrix_destroy(d); 
  nsp_matrix_destroy(v); 
  return FAIL;
}

#if 0 
static int intdsyev(NspMatrix *A,NspMatrix **d,char flag);
static int intzheev(NspMatrix *A,NspMatrix **d,char flag);
#endif 

static int intdsyevr(NspMatrix *A,NspMatrix **d,char flag);
static int intzheevr(NspMatrix *A,NspMatrix **d, char flag);

/**
 * nsp_spec_sym:
 * @A: (input/output) a square real symetric or complex hermitian matrix  
 * @d: (output) a real vector : the eigenvalues of the matrix @A
 * @flag: if flag == 'V',  the eigenvectors are returned in @A
 * 
 * Computes eigenvalues and (if flag == 'V') the eigenvectors of the matrix @A
 * (in this case eigenvectors are returned in @A).
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_spec_sym(NspMatrix *A,NspMatrix **d,char flag)
{
  int m=A->m, n=A->m;
  /*  A = [] return empty matrices */ 
  if ( A->mn == 0 ) 
    {
      if (( *d =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      return OK ; 
    }
  
  if (m != n) 
    { 
      Scierror("Error: first argument of spec should be square and it is (%dx%d)\n", 
	       m,n);
      return FAIL;
    }

  if ( nsp_mat_have_nan_or_inf(A) )
    {
      Scierror("Error: nan or inf in spec first argument\n"); 
      return FAIL;
    }
  
  if ( A->rc_type == 'r' ) 
    return  intdsyevr(A,d,flag); 
  else 
    return  intzheevr(A,d,flag); 
}

#if 0
/* unuued */
static int intdsyev(NspMatrix *A,NspMatrix **d,char flag)
{
  int n=A->n;
  int info, lwork;
  NspMatrix *wr;
  double *dwork;
  double qwork[1];

  if ( (wr=nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT ) return FAIL;

  lwork =  -1;
  C2F(dsyev)((flag == 'V' ) ? "V" : "N","U" , &n,A->R, &n,wr->R,qwork,&lwork, &info, 1L, 1L);
  lwork = (int) qwork[0];
  if ( (dwork=nsp_alloc_work_doubles(lwork)) == NULL ) {nsp_matrix_destroy(wr); return FAIL;}
  
  C2F(dsyev)((flag == 'V' ) ? "V" : "N","U", &n, A->R, &n, wr->R, dwork, &lwork, &info, 1L, 1L);
  
  if (info != 0) 
    {
      Scierror("Error: convergence problem in dsyev\n"); 
      nsp_matrix_destroy(wr);
      FREE(dwork);
      return FAIL;
    }

  *d = wr ; 
  FREE(dwork);
  return OK;
}
#endif 

static int intdsyevr(NspMatrix *A,NspMatrix **d, char flag)
{
  int n=A->n, il=1, iu=n;
  int info=0, lwork, m, liwork, qiwork[1], *iwork=NULL, *isuppz=NULL;
  NspMatrix *wr;
  double *dwork=NULL, abstol=0.0, qwork[1], *Z=NULL, vl=0, vu=0;


  if ( (wr=nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT ) return FAIL;
  if ( (isuppz =nsp_alloc_work_int(2*n)) == NULL ) goto err;
  if ( flag == 'V' )
    if ( (Z =nsp_alloc_work_doubles(n*n)) == NULL ) goto err;

  lwork = -1; liwork = -1;
  C2F(dsyevr)((flag == 'V' ) ? "V" : "N","A", "U" , &n,A->R, &n,&vl,&vu,
	     &il,&iu,&abstol, &m, wr->R, Z, &n, isuppz, qwork, &lwork,
	     qiwork, &liwork, &info, 1L, 1L, 1L);

  lwork = (int) qwork[0];
  liwork = qiwork[0];
  if ( (dwork =nsp_alloc_work_doubles(lwork)) == NULL ) goto err;
  if ( (iwork =nsp_alloc_work_int(liwork)) == NULL ) goto err;
  

  C2F(dsyevr)((flag == 'V' ) ? "V" : "N","A", "U" , &n,A->R, &n,&vl,&vu,
	     &il, &iu,&abstol, &m, wr->R, Z, &n, isuppz, dwork, &lwork,
	     iwork, &liwork, &info, 1L, 1L, 1L);

  if (info != 0) 
    {
      Scierror("Error: convergence problem in dsyevr\n"); 
      goto err;
    }

  *d = wr ; 
  FREE(dwork); FREE(iwork); FREE(isuppz);
  if ( flag == 'V' )
    {
      FREE(A->R); A->R = Z;
    }
  return OK;

 err:
  nsp_matrix_destroy(wr);
  FREE(dwork);
  FREE(iwork);
  FREE(isuppz);
  FREE(Z);
  return FAIL;
}

#if 0 
/* unused */
static int intzheev(NspMatrix *A,NspMatrix **d,char flag)
{
  int n=A->n;
  int info, lwork;
  NspMatrix *wr=NULLMAT;
  double *rwork=NULL;
  doubleC *cwork=NULL, qwork[1];

  if ( (wr=nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT ) return FAIL;

  if ( (rwork=nsp_alloc_work_doubles(n*3-2)) == NULL ) goto err;

  lwork = -1;
  C2F(zheev)((flag == 'V' ) ? "V" : "N","U" , &n, A->C, &n,wr->R, qwork, &lwork, rwork, &info, 1L, 1L);
  lwork = (int) qwork[0].r;
  if ( (cwork=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;
  
  C2F(zheev)((flag == 'V' ) ? "V" : "N","U" , &n, A->C, &n,wr->R, cwork, &lwork, rwork, &info, 1L, 1L);
  
  if (info != 0) 
    {
      Scierror("Error: convergence problem in zheev\n"); 
      goto err;
    }

  *d = wr ; 
  FREE(rwork);
  FREE(cwork);
  return OK;

 err:
  nsp_matrix_destroy(wr); 
  FREE(rwork);
  FREE(cwork);
  return FAIL;
}
#endif 

static int intzheevr(NspMatrix *A,NspMatrix **d, char flag)
{
  int n=A->n, il=1, iu=n;
  int info=0, lwork, lrwork, m, liwork, qiwork[1], *iwork=NULL, *isuppz=NULL;
  NspMatrix *wr;
  double *rwork=NULL, abstol=0.0, qrwork[1], vl=0, vu=0;
  doubleC *Z=NULL, *work=NULL, qwork[1];

  if ( (wr=nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT ) return FAIL;
  if ( (isuppz =nsp_alloc_work_int(2*n)) == NULL ) goto err;
  if ( flag == 'V' )
    if ( (Z =nsp_alloc_work_doubleC(n*n)) == NULL ) goto err;

  lwork = -1; lrwork = -1; liwork = -1;
  C2F(zheevr)((flag == 'V' ) ? "V" : "N","A", "U" , &n,A->C, &n,&vl,&vu,
	     &il,&iu,&abstol, &m, wr->R, Z, &n, isuppz, qwork, &lwork,
	     qrwork, &lrwork, qiwork, &liwork, &info, 1L, 1L, 1L);

  lwork = (int) qwork[0].r;
  lrwork = (int) qrwork[0];
  liwork = qiwork[0];
  if ( (work =nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;
  if ( (rwork =nsp_alloc_work_doubles(lrwork)) == NULL ) goto err;
  if ( (iwork =nsp_alloc_work_int(liwork)) == NULL ) goto err;

  C2F(zheevr)((flag == 'V' ) ? "V" : "N","A", "U" , &n,A->C, &n,&vl,&vu,
	     &il, &iu,&abstol, &m, wr->R, Z, &n, isuppz, work, &lwork,
	     rwork, &lrwork, iwork, &liwork, &info, 1L, 1L, 1L);

  if (info != 0) 
    {
      Scierror("Error: convergence problem in zheevr\n"); 
      goto err;
    }

  *d = wr ; 
  FREE(work); FREE(rwork); FREE(iwork); FREE(isuppz);
  if ( flag == 'V' )
    {
      FREE(A->C); A->C = Z;
    }
  return OK;

 err:
  nsp_matrix_destroy(wr);
  FREE(work);
  FREE(rwork);
  FREE(iwork);
  FREE(isuppz);
  FREE(Z);
  return FAIL;
}

static int intdggev(NspMatrix *A,NspMatrix *B,NspMatrix **Vl,NspMatrix **Vr,NspMatrix **alpha,NspMatrix **beta);
static int intzggev(NspMatrix *A,NspMatrix *B,NspMatrix **Vl,NspMatrix **Vr,NspMatrix **alpha,NspMatrix **beta);

/**
 * nsp_gspec:
 * @A:  (input) a square real or complex matrix (@A is not modified)
 * @B:  (input) a square real or complex matrix (@B is not modified)
 * @Vl: (output if Vl is not NULL on entry) a real or complex matrix
 * @Vr: (output if Vl is not NULL on entry) a real or complex matrix
 * @alpha: (output) a real or complex vector
 * @beta:  (output) a real or complex vector
 * 
 * Computes the generalized eigenvalues lambda = alpha/beta and (eventually)
 * the left (@Vl) and right (@Vr) eigenvectors of the generalized 
 * eigenproblem :
 *
 *           @A @Vr = lambda @B @Vr
 *
 *          @Vl' @A = lambda @Vl' @B
 *
 * right eigenvectors are computed if @Vr is not NULL   
 *
 * left eigenvectors are computed if @Vl is not NULL   
 *
 * Return value: %OK or %FAIL
 **/

int nsp_gspec(NspMatrix *A, NspMatrix *B, NspMatrix **Vl, NspMatrix **Vr,
	      NspMatrix **alpha, NspMatrix **beta)
{
  NspMatrix *AA = NULLMAT, *BB = NULLMAT;

  if (A->m != A->n) 
    { 
      Scierror("Error: first argument of gspec should be square and it is (%dx%d)\n", 
	       A->m, A->n);
      return FAIL;
    }

  if (B->m != B->n) 
    { 
      Scierror("Error: second argument of gspec should be square and it is (%dx%d)\n", 
	       B->m, B->n);
      return FAIL;
    }

  if ( A->m != B->m ) 
    {
      Scierror("gspec: first and second arguments must have equal size\n");
      return FAIL;
    }

  /* A = [] return empty matrices */ 
  if ( A->mn == 0 ) 
    {
      if ( Vl != NULL)
	{
	  if ( (*Vl =nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT ) return FAIL;
	}
      if ( Vr != NULL)
	{
	  if ( (*Vr =nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT ) return FAIL;
	} 
      if ( (*alpha =nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT ) return FAIL;
      if ( (*beta =nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT ) return FAIL;
      return OK ; 
    }

  if ( nsp_mat_have_nan_or_inf(A) ||  nsp_mat_have_nan_or_inf(B) )
    {
      Scierror("Error: nan or inf in gspec first argument\n"); 
      return FAIL;
    }

  if ( A->rc_type == B->rc_type )
    {
      if ( (AA =nsp_matrix_copy(A)) == NULLMAT ) return FAIL;
      if ( (BB =nsp_matrix_copy(B)) == NULLMAT ) goto err; 
    }
  else if ( A->rc_type == 'r' )
    {
      if ( (AA =nsp_mat_copy_and_complexify(A)) == NULLMAT ) return FAIL;
      if ( (BB =nsp_matrix_copy(B)) == NULLMAT ) goto err; 
    }
  else
    {
      if ( (AA =nsp_matrix_copy(A)) == NULLMAT ) return FAIL;
      if ( (BB =nsp_mat_copy_and_complexify(B)) == NULLMAT ) goto err; 
    }

  if ( AA->rc_type == 'r' ) 
    {
      if ( intdggev(AA,BB,Vl,Vr,alpha,beta) == FAIL ) goto err;
    }
  else 
    {
      if ( intzggev(AA,BB,Vl,Vr,alpha,beta) == FAIL ) goto err;
    }
  
  nsp_matrix_destroy(AA);
  nsp_matrix_destroy(BB);
  return OK;

 err:
  nsp_matrix_destroy(AA);
  nsp_matrix_destroy(BB);
  return FAIL;
}


static int intdggev(NspMatrix *A, NspMatrix *B, NspMatrix **Vl, NspMatrix **Vr, 
		    NspMatrix **Alpha, NspMatrix **Beta)
{
  double *dwork=NULL, qwork[1], *vlr=NULL, *vrr=NULL; 
  char *jobVl ="N", *jobVr = "N", type = 'r';
  NspMatrix *alpha=NULLMAT, *alphar=NULLMAT, *alphai=NULLMAT, *beta=NULLMAT, *vl=NULLMAT, *vr=NULLMAT; 
  int info, lwork, i, j;  

  int n = A->n;

  if ( (beta =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT ) return FAIL;
  if ( (alphar =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT ) goto err;
  if ( (alphai =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT ) goto err;

  if ( Vl != NULL ) 
    {
      if ( (vl =nsp_matrix_create(NVOID,'r',n,n)) == NULLMAT ) goto err;
      jobVl = "V"; vlr = vl->R;
    }

  if ( Vr != NULL ) 
    {
      if ( (vr =nsp_matrix_create(NVOID,'r',n,n)) == NULLMAT ) goto err;
      jobVr = "V"; vrr = vr->R;
    }

  lwork = -1; 
  /* call to query work size */
  C2F(dggev)(jobVl,jobVr, &n, A->R, &n, B->R, &n, alphar->R, alphai->R, beta->R, 
	     vlr, &n, vrr, &n, qwork, &lwork, &info, 1L, 1L); 
  lwork = (int) qwork[0];
  if ( (dwork=nsp_alloc_work_doubles(lwork)) == NULL ) goto err;

  C2F(dggev)(jobVl,jobVr, &n, A->R, &n, B->R, &n, alphar->R, alphai->R, beta->R,
	     vlr, &n, vrr, &n, dwork, &lwork, &info, 1L, 1L);

  if (info < 0 || info >= n) 
    {
      if (info == n) 
	Scierror("Error: gspec, the QZ iteration completly failed\n");
      else if (info == n + 1) 
	Scierror("Error: gspec, other than QZ iteration failed in dhgeqz\n");
      else if (info == n + 2) 
	Scierror("Error: gspec, error return from dtgevc\n");
      else
	Scierror("Error: gspec, wrong argument (%d) in dggev call\n",-info);
      goto err;
    }

  /* return (alphar + %i*alphai) and beta */
  /* result is real ? or complex */ 
  for (i = 0 ; i < n ; ++i) { if (alphai->R[i] != 0.0) { type = 'c'; break;}}

  if ( type == 'c' )   /* alpha = (alphar,alphai) */ 
    {
      if ( (alpha =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT ) goto err;
      for (i = 0 ; i < n ; ++i) 
	{ 
	  alpha->C[i].r = alphar->R[i]; 
	  alpha->C[i].i = alphai->R[i]; 
	}
    }

  if ( info > 0 ) /* special case of partial convergence */
    {
      Sciprintf("Warning: gspec, the QZ iteration failed. No eigenvectors have been calculated\n");
      Sciprintf("\treturn only alpha(i) and beta(i) which should be correct\n");
      /* reduce size of the alpha and beta vectors */
      memmove(&(beta->R[0]), &(beta->R[info]), (n-info)*sizeof(double));
      nsp_matrix_resize(beta, n-info, 1);
      if ( type == 'c' )
	{
	  memmove(&(alpha->C[0]), &(alpha->C[info]), (n-info)*sizeof(doubleC));
	  nsp_matrix_resize(alpha, n-info, 1);
	}
      else
	{
	  memmove(&(alphar->R[0]), &(alphar->R[info]), (n-info)*sizeof(double));
	  nsp_matrix_resize(alphar, n-info, 1);
	}
      if ( Vl != NULL )
	if ( (*Vl=nsp_matrix_create(NVOID,'r',0,0)) == NULLMAT ) goto err;
      if ( Vr != NULL )
	if ( (*Vr=nsp_matrix_create(NVOID,'r',0,0)) == NULLMAT ) goto err;
      nsp_matrix_destroy(vl); 
      nsp_matrix_destroy(vr);
    }
  else
    {
      if ( Vr != NULL && type == 'c' ) /* fill Vr if requested */ 
	{
	  if (nsp_mat_complexify(vr,0.0) == FAIL ) goto err;
	  j = 0;
	  while (j < n) 
	    {
	      if( alphai->R[j] != 0.0 ) 
		{
		  for ( i = 0;  i < n ; ++i) 
		    { 
		      int k = i+j*n ;
		      vr->C[k].i = vr->C[k+n].r; 
		      vr->C[k+n].r = vr->C[k].r;
		      vr->C[k+n].i = - vr->C[k].i;
		    }
		  j++;
		}
	      j++;
	    }
	}
      if ( Vl != NULL && type == 'c' ) /* fill Vl if requested */ 
	{
	  if (nsp_mat_complexify(vl,0.0) == FAIL ) goto err;
	  j = 0;
	  while (j < n) 
	    {
	      if( alphai->R[j] != 0.0 ) 
		{
		  for ( i = 0;  i < n ; ++i) 
		    { 
		      int k = i+j*n ;
		      vl->C[k].i = vl->C[k+n].r; 
		      vl->C[k+n].r = vl->C[k].r;
		      vl->C[k+n].i = - vl->C[k].i;
		    }
		  j++;
		}
	      j++;
	    }
	}
      if ( Vr != NULL ) *Vr = vr;
      if ( Vl != NULL ) *Vl = vl;
    }
  if ( type == 'r' ) 
    *Alpha = alphar; 
  else 
    {
      *Alpha = alpha; nsp_matrix_destroy(alphar);
    }
  *Beta = beta;
  nsp_matrix_destroy(alphai); 
  FREE(dwork);
  return OK;

 err:
  FREE(dwork);
  nsp_matrix_destroy(alpha);
  nsp_matrix_destroy(alphar);
  nsp_matrix_destroy(alphai);
  nsp_matrix_destroy(beta);
  nsp_matrix_destroy(vl);
  nsp_matrix_destroy(vr);
  return FAIL;
} 

static int intzggev(NspMatrix *A, NspMatrix *B, NspMatrix **Vl, NspMatrix **Vr, 
		    NspMatrix **Alpha, NspMatrix **Beta)
{
  double *dwork=NULL;
  doubleC qwork[1], *work=NULL, *vlc=NULL, *vrc=NULL; 
  char *jobVl ="N", *jobVr = "N";
  NspMatrix *alpha=NULLMAT, *beta=NULLMAT, *vl=NULLMAT, *vr=NULLMAT; 
  int info, lwork;  

  int n = A->n;

  if ( (dwork =nsp_alloc_work_doubles(8*n)) == NULL ) return FAIL;
  if ( (beta =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT ) goto err;
  if ( (alpha =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT ) goto err;

  if ( Vl != NULL ) 
    {
      if ( (vl =nsp_matrix_create(NVOID,'c',n,n)) == NULLMAT ) goto err;
      jobVl = "V"; vlc = vl->C;
    }

  if ( Vr != NULL ) 
    {
      if ( (vr =nsp_matrix_create(NVOID,'c',n,n)) == NULLMAT ) goto err;
      jobVr = "V"; vrc = vr->C;
    }

  lwork = -1; 
  /* first call to query work size */
  C2F(zggev)(jobVl,jobVr, &n, A->C, &n, B->C, &n, alpha->C, beta->C, 
	     vlc, &n, vrc, &n, qwork, &lwork, dwork, &info, 1L, 1L); 
  lwork = (int) qwork[0].r;
  if ( (work=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;

  C2F(zggev)(jobVl,jobVr, &n, A->C, &n, B->C, &n, alpha->C, beta->C, 
	     vlc, &n, vrc, &n, work, &lwork, dwork, &info, 1L, 1L); 

  if (info < 0 || info >= n) 
    {
      if (info == n) 
	Scierror("Error: gspec, the QZ iteration completly failed\n");
      else if (info == n + 1) 
	Scierror("Error: gspec, other than QZ iteration failed in zhgeqz\n");
      else if (info == n + 2) 
	Scierror("Error: gspec, error return from ztgevc\n");
      else
	Scierror("Error: gspec, wrong argument (%d) in zggev call\n",-info);
      goto err;
    }

  if ( info > 0 ) /* special case of partial convergence */
    {
      Sciprintf("Warning: gspec, the QZ iteration failed. No eigenvectors have been calculated\n");
      Sciprintf("\treturn only alpha(i) and beta(i) which should be correct\n");
      /* reduce size of the alpha and beta vectors */
      memmove(&(beta->C[0]), &(beta->C[info]), (n-info)*sizeof(doubleC));
      nsp_matrix_resize(beta, n-info, 1);
      memmove(&(alpha->C[0]), &(alpha->C[info]), (n-info)*sizeof(doubleC));
      nsp_matrix_resize(alpha, n-info, 1);

      if ( Vl != NULL )
	if ( (*Vl=nsp_matrix_create(NVOID,'r',0,0)) == NULLMAT ) goto err;
      if ( Vr != NULL )
	if ( (*Vr=nsp_matrix_create(NVOID,'r',0,0)) == NULLMAT ) goto err;
      nsp_matrix_destroy(vl); 
      nsp_matrix_destroy(vr);
    }
  else
    {
      if ( Vr != NULL ) *Vr = vr;
      if ( Vl != NULL ) *Vl = vl;
    }

  *Alpha = alpha;
  *Beta = beta;
  FREE(work);
  FREE(dwork);
  return OK;

 err:
  FREE(dwork);
  FREE(work);
  nsp_matrix_destroy(alpha);
  nsp_matrix_destroy(beta);
  nsp_matrix_destroy(vl);
  nsp_matrix_destroy(vr);
  return FAIL;
} 


static int intdgetri(NspMatrix *A, double *rcond, Boolean warning);
static int intzgetri(NspMatrix *A, double *rcond, Boolean warning);
static int inttrcon(NspMatrix *A, char tri_type, double *rcond);

/**
 * nsp_inv:
 * @A: (input/output) a square real or complex matrix 
 * @rcond: a double pointer (can be set to NULL).
 * @warning: boolean 
 * 
 * Computes the inverse matrix of the matrix @A. nsp_inv tests if
 * @A is lower or upper triangular (to speed up the computation in theses
 * cases). @A is modified and stores its inverse matrix on output.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_inv(NspMatrix *A, double *rcond, Boolean warning) 
{
  char tflag;  /* N : non triangular, U : upper triangular, L : lower triangular */
  int n=A->n, m = A->m, info;

  /*  A = [] return empty matrices */ 
  if ( A->mn == 0 )  return OK ; 
  
  if (m != n) 
    { 
      Scierror("Error: first argument of inv should be square and it is (%dx%d)\n", m, n);
      return FAIL;
    }

  if ( nsp_mat_is_upper_triangular(A) )
    tflag = 'U';
  else if ( nsp_mat_is_lower_triangular(A) )
    tflag = 'L';
  else 
    tflag = 'N';

  if ( tflag == 'N' )
    {
      if ( A->rc_type == 'r')
	return intdgetri(A, rcond, warning);
      else
	return intzgetri(A, rcond, warning);
    }
  else  /* triangular case */
    {
      if ( rcond != NULL )
	inttrcon(A, tflag, rcond);
      if ( A->rc_type == 'r')
	C2F(dtrtri) (&tflag, "N", &n, A->R, &n, &info, 1L, 1L);
      else
	C2F(ztrtri) (&tflag, "N", &n, A->C, &n, &info, 1L, 1L);

      if (info == 0) 
	{
	  if ( warning && rcond != NULL && *rcond <= n*nsp_dlamch("eps") )
	    Sciprintf("Warning: matrix is numerically singular (rcond = %e)\n",*rcond);
 	  return OK;
	} 
      else 
	{
	  Scierror("inv: matrix is singular\n");
	  return FAIL;
	}
    }
}

static int intdgetri(NspMatrix *A, double *rcond, Boolean warning)
{
  int *ipiv=NULL, *iwork=NULL;
  double *dwork=NULL, qwork[1], anorm;
  int info, lwork, n=A->n ;

  if ( (ipiv = nsp_alloc_int(n)) == NULL ) goto err;

  if ( rcond != NULL )
    anorm = C2F(dlange)("1", &n, &n, A->R, &n, NULL, 1L);

  C2F(dgetrf)(&n, &n, A->R, &n, ipiv, &info);

  if (info != 0) 
    {
      Scierror("inv: matrix is singular\n");
      goto err;
    };

  lwork = -1;
  C2F(dgetri)(&n, A->R, &n, ipiv, qwork, &lwork, &info);
  lwork = (int) qwork[0];
  lwork = Max ( lwork, 4*n );
  if ( (dwork =nsp_alloc_work_doubles(lwork)) == NULL ) goto err;

  if ( rcond != NULL )
    {
      if ( (iwork = nsp_alloc_int(n)) == NULL ) goto err;
      C2F(dgecon)("1", &n, A->R, &n, &anorm, rcond, dwork, iwork, &info, 1L);
      if ( warning && *rcond <= n*nsp_dlamch("eps") )
	Sciprintf("Warning: matrix is numerically singular (rcond = %e)\n",*rcond);
    }

  C2F(dgetri)(&n, A->R, &n, ipiv, dwork, &lwork, &info);

  FREE(dwork); FREE(ipiv); FREE(iwork);
  return OK;

 err:
  FREE(dwork); FREE(ipiv); FREE(iwork);
  return FAIL;
} 

static int intzgetri(NspMatrix *A, double *rcond, Boolean warning)
{
  int *ipiv=NULL;
  doubleC *cwork=NULL, qwork[1];
  double anorm;
  int info, lwork, n = A->n;

  if ( (ipiv = nsp_alloc_int(n)) == NULL ) goto err;

  if ( rcond != NULL )
    anorm = C2F(zlange)("1", &n, &n, A->C, &n, NULL, 1L);

  C2F(zgetrf)(&n, &n, A->C, &n, ipiv, &info);
  if (info != 0) 
    {
      Scierror("inv: matrix is singular\n");
      goto err;
    };

  lwork = -1;
  C2F(zgetri)(&n, A->C, &n, ipiv, qwork, &lwork, &info);
  lwork = (int) qwork[0].r;
  lwork = Max ( lwork, 3*n );

  if ( (cwork=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;

  if ( rcond != NULL )
    {
      C2F(zgecon)("1", &n, A->C, &n, &anorm, rcond, cwork, (double *) &(cwork[2*n]), &info, 1L);
      if ( warning && *rcond <= n*nsp_dlamch("eps") )
	Sciprintf("Warning: matrix is numerically singular (rcond = %e)\n",*rcond);
    }

  C2F(zgetri)(&n, A->C, &n, ipiv, cwork, &lwork, &info);

  FREE(cwork); FREE(ipiv);
  return OK;

 err:
  FREE(cwork); FREE(ipiv);
  return FAIL;
} 


static int intdgecon(NspMatrix *A,double *rcond);
static int intzgecon(NspMatrix *A,double *rcond);

/**
 * nsp_rcond:
 * @A: (input) a real or complex square matrix (A is modified)
 * @rcond: (output) an estimate of the reciprocal condition number of the matrix @A
 * 
 * Computes an estimate of the reciprocal condition number of the matrix @A
 * using 1-norm, that is an estimate of :
 *                     
 *    1 / ( ||A||_1 ||inv(A)||_1 )  
 * 
 * nsp_rcond tests if @A is lower or upper triangular to speed up the computation in theses
 * cases.
 *
 * Return value: %OK or %FAIL
 **/

int nsp_rcond(NspMatrix *A,double *rcond)
{
  NspMatrix *Ac;
  int stat;

  /*  A = [] return empty matrices */ 
  if ( A->mn == 0 )  return OK ; 
  
  if (A->m != A->n) 
    { 
      Scierror("Error: first argument of rcond should be square and it is (%dx%d)\n", 
	       A->m,A->n);
      return FAIL;
    }

  if ( nsp_mat_is_upper_triangular(A) )
    return inttrcon(A, 'u', rcond);
  else if ( nsp_mat_is_lower_triangular(A) )
    return inttrcon(A, 'l', rcond);
  else 
    {
      if ( (Ac = nsp_matrix_copy(A)) == NULLMAT ) return FAIL;
      if ( A->rc_type == 'r') 
	stat = intdgecon(Ac, rcond);
      else 
	stat = intzgecon(Ac, rcond);
      nsp_matrix_destroy(Ac);
      return stat;
    }
}

static int intdgecon(NspMatrix *A,double *rcond) 
{
  int *iwork=NULL;
  double *dwork=NULL, anorm;
  int info, n=A->n;
  *rcond = 0.0;

  if ( (iwork=nsp_alloc_work_int(n)) == NULL ) return FAIL;
  if ( (dwork =nsp_alloc_work_doubles(4*n)) == NULL ) goto err;

  anorm = C2F(dlange)("1", &n, &n, A->R, &n, NULL, 1L);
  C2F(dgetrf)(&n, &n, A->R, &n, iwork , &info);
  if (info == 0) 
    C2F(dgecon)("1", &n, A->R, &n, &anorm, rcond, dwork, iwork, &info, 1L);
  else if (info > 0)   /* there is a zero pivot */
    *rcond = 0.0;
  else    /* must not arrive */
    {
      Scierror("Error: something wrong in dgetrf\n"); 
      goto err;
    }
  
  FREE(dwork); FREE(iwork); 
  return OK;

 err:
  FREE(dwork); FREE(iwork); 
  return FAIL;
} 

static int intzgecon(NspMatrix *A,double *rcond) 
{
  double anorm, *rwork=NULL;
  doubleC *cwork=NULL;
  int *iwork=NULL, info, n=A->n;

  *rcond=0.0;

  if ( (iwork=nsp_alloc_work_int(n)) == NULL ) return FAIL;
  if ( (rwork=nsp_alloc_work_doubles(2*n)) == NULL ) goto err;
  if ( (cwork=nsp_alloc_work_doubleC(2*n)) == NULL ) goto err;

  anorm = C2F(zlange)("1", &n, &n, A->C, &n, NULL, 1L);
  C2F(zgetrf)(&n, &n, A->C, &n, iwork , &info);
  if (info == 0) 
    C2F(zgecon)("1", &n, A->C, &n, &anorm, rcond, cwork, rwork, &info, 1L);
  else if (info > 0)   /* there is a zero pivot */
    *rcond = 0.0;
  else                 /* must not arrive */
    {
      Scierror("Error: something wrong in zgetrf\n"); 
      return FAIL;
    }
  
  FREE(cwork); FREE(rwork); FREE(iwork); 
  return OK;

 err:
  FREE(cwork); FREE(rwork); FREE(iwork); 
  return FAIL;
} 

static int inttrcon(NspMatrix *A, char tri_type, double *rcond1)
{  
  int m = A->m, info;
  double *rwork=NULL;
  doubleC *cwork=NULL;
  int *iwork=NULL;

  if ( A->rc_type == 'r' )
    {
      iwork = nsp_alloc_work_int(m);
      rwork = nsp_alloc_work_doubles(3*m);
      if ( iwork == NULL  ||  rwork == NULL ) goto err;
      C2F(dtrcon) ("1", &tri_type, "N", &m, A->R, &m, rcond1, rwork, iwork, &info, 1, 1, 1);
      FREE(rwork); FREE(iwork);
    }
  else
    {
      rwork = nsp_alloc_work_doubles(m);
      cwork = nsp_alloc_work_doubleC(2*m);
      if ( rwork == NULL  ||  cwork == NULL ) goto err;
      C2F(ztrcon) ("1", &tri_type, "N", &m, A->C, &m, rcond1, cwork, rwork, &info, 1, 1, 1);
      FREE(cwork); FREE(rwork);
    }

  return OK;

 err:
  FREE(iwork); FREE(cwork); FREE(rwork);
  return FAIL;
}

/**
 * nsp_cholesky:
 * @A: (input/output) a symetric real or complex hermitian matrix (the upper
 *     triangle of @A is used) which must be positive definite.
 * @minor: an integer 
 * 
 * Computes the Cholesky factorization of the matrix @A (@A must be positive
 * definite) i.e A = R'*R (R an upper triangular matrix with positive diagonal elements).
 * The matrix R is returned in @A. If the matrix is not positive definite the function 
 * returns %FAIL, except if minor is equal to one on entry. In that case the order 
 * of the leading minor which is not positive definite is returned in @minor.
 * In that case R1=R(1:q,1:q) is such that R1'*R1=A(1:q,1:q) where q=p-1;
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_cholesky(NspMatrix *A, int *minor) 
{
  int info, m = A->m, n = A->n ;

  /*  A = [] return empty matrices */ 
  if ( A->mn == 0 )  return OK ; 
  
  if (m != n) 
    { 
      Scierror("Error: first argument of chol should be square and it is (%dx%d)\n", m,n);
      return FAIL;
    }
  if ( A->rc_type == 'r' ) 
    C2F(dpotrf)("U", &n,A->R, &n, &info, 1L);
  else 
    C2F(zpotrf)("U", &n,A->C, &n, &info, 1L);
  if (info != 0) 
    {
      if (info > 0)
	{
	  if ( *minor == -1 ) 
	    {
	      *minor = info ;
	    }
	  else 
	    {
	      *minor = info ;
	      Scierror("Error: matrix is not positive definite (minor %d)\n",info); 
	      return FAIL;
	    }
	}
      else 
	{
	  Scierror("Eroor: the %d-th argument of [dz]potrf had an illegal value\n",-info);
	  return FAIL;
	}
    }
  nsp_mat_triu(A,0);  
  return OK;
} 


static NspMatrix * intzdet(NspMatrix *A,char mode);
static NspMatrix * intddet(NspMatrix *A,char mode);

/**
 * nsp_det:
 * @A: (input) a square real or complex matrix. @A is modified.
 * @mode: 
 * 
 * returns d=det or [e,m] with det = m*10^e
 * according to mode 
 * 
 * Return value: 
 **/

NspMatrix * nsp_det(NspMatrix *A,char mode)
{
  /*  A = [] return empty matrices */ 
  if ( A->mn == 0 ) 
    {
      NspMatrix *det; 
      if ( (det=nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT ) 
	return NULL;
      return det ; 
    }
  
  if (A->m != A->n) 
    { 
      Scierror("Error: first argument for det should be square and it is (%dx%d)\n", 
	       A->m, A->n);
      return NULL;
    }

  if ( A->rc_type == 'r' ) 
    return  intddet(A,mode); 
  else 
    return  intzdet(A,mode); 
}


static NspMatrix * intddet(NspMatrix *A,char mode)
{
  NspMatrix *det; 
  int info, ix, n=A->n, *ipiv=NULL;

  if ( (ipiv=nsp_alloc_work_int(n)) == NULL ) return NULLMAT;

  C2F(dgetrf)(&n, &n, A->R, &n, ipiv , &info);
  
  if (mode == 'd')  /* det(A) */
    { 
      double dx = 1.;
      for (ix = 0; ix < n ; ++ix)  /* loop on diag(A) */ 
	{
	  if ( ipiv[ix] != ix+1 ) dx = -dx; 
	  dx *= A->R[ix*(n+1)];
	}
      if ( (det=nsp_matrix_create(NVOID,A->rc_type,1,1)) == NULLMAT ) goto err;
      det->R[0] = dx;
    } 
  else   /*  [e,m]=det(A) */
    {   
      const double one=1.0, ten=10.0;
      double dx=1.0, e=0.0;
      for (ix = 0; ix < n; ++ix) 
	{
	  if ( ipiv[ix] != ix+1) dx = -dx; 
	  dx *= A->R[ix*(n+1)];
	  if (dx == 0.0) break; 
	  while ( Abs(dx) < one ) { dx *= ten; e -= one; }
	  /* attention si Inf on va boucler à l'infini */
	  while ( Abs(dx) >= ten ) { dx /= ten; e += one; }
	}
      if (( det =nsp_matrix_create(NVOID,A->rc_type,1,2)) == NULLMAT) goto err;
      det->R[0]= e; 
      det->R[1]= dx; 
    } 
  FREE(ipiv);
  return det;

 err:
  FREE(ipiv);
  return NULLMAT;
}
  
static NspMatrix * intzdet(NspMatrix *A,char mode)
{
  NspMatrix *det;
  int info, ix, n=A->n, *ipiv=NULL;

  if ( (ipiv=nsp_alloc_work_int(n)) == NULL ) return NULLMAT;

  C2F(zgetrf)(&n, &n, A->C, &n, ipiv, &info);
  
  if (mode == 'd' )  /* det(A) */ 
    {   
      doubleC dx={1.0,0.0};
      for (ix = 0; ix < n ; ++ix)  /* loop on diag(A) */ 
	{
	  if ( ipiv[ix] != ix+1 ) { dx.r = -dx.r;  dx.i = -dx.i;}
	  nsp_prod_c(&dx,&A->C[ix*(n+1)]);
	}
      if ( (det=nsp_matrix_create(NVOID,A->rc_type,1,1)) == NULLMAT) goto err;
      det->C[0] = dx ;
    } 
  else              /* [e,m]=det(A) */
    {   
      const double one=1.0, ten=10.0;
      doubleC dx={1.0,0.0};
      double e = 0.0;
      for (ix = 0; ix < n; ++ix) 
	{
	  if ( ipiv[ix] != ix+1) { dx.r = -dx.r;  dx.i = -dx.i;}
	  nsp_prod_c(&dx,&A->C[ix*(n+1)]);
	  if ( nsp_abs_c(&dx) == 0.0) break;  /* FIXME : on peut ecrire dx.r==0 && dx.i==0 */
	  while ( nsp_abs_c(&dx) < one ) { dx.r *= ten; dx.i *= ten; e -= one; }
	  /* attention si Inf on va boucler à l'infini */
	  while ( nsp_abs_c(&dx) >= ten ) { dx.r /= ten; dx.i /= ten; e += one; }
	}
      if ( (det=nsp_matrix_create(NVOID,A->rc_type,1,2)) == NULLMAT ) goto err;
      (det)->C[0].r = e; (det)->C[0].i = 0.0; (det)->C[1] = dx; 
    }
  FREE(ipiv);
  return det;

 err:
  FREE(ipiv);
  return NULLMAT;
} 


static int intdgebal(NspMatrix *A,NspMatrix **D);
static int intzgebal(NspMatrix *A,NspMatrix **D);

/**
 * nsp_balanc:
 * @A: The matrix to balance 
 * @D: The similarity transformation X such that inv(X)*A*X
 * 
 * Balances a general real matrix @A using Lapack dgebal or zgebal. 
 * Balancing may reduce the 1-norm of the matrix, and improve the accuracy
 * of the computed eigenvalues and/or eigenvectors. The balanced matrix 
 * is returned in @A and the similarity transformation is returned in @D. 
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_balanc(NspMatrix *A,NspMatrix **D)
{

  if ( A->mn == 0 )   /*  A = [] return empty matrices */ 
    {
      if ( (*D=nsp_matrix_create(NVOID,A->rc_type,0,0)) == NULLMAT ) return FAIL;
      return OK ; 
    }
  
  if (A->m != A->n) 
    { 
      Scierror("Error: first argument of balanc should be square and it is (%dx%d)\n", 
	       A->m,A->n);
      return FAIL;
    }

  if ( nsp_mat_have_nan_or_inf(A) )
    {
      Scierror("Error: nan or inf in balanc first argument\n"); 
      return FAIL;
    }

  if ( A->rc_type == 'r' ) 
    return  intdgebal(A,D); 
  else 
    return  intzgebal(A,D); 
}

static int intdgebal(NspMatrix *A,NspMatrix **D)
{
  int n = A->n, info;
  double *work=NULL; 
  int ilo, ihi;

  if ( (*D =nsp_mat_eye(n,n)) == NULLMAT ) return FAIL;
  if ( (work = nsp_alloc_work_doubles(n)) == NULL ) goto err;

  C2F(dgebal)("B", &n, A->R, &n, &ilo, &ihi, work, &info, 1L);
  if (info != 0) 
    {
      Scierror("Error: something wrong in dgebal\n");
      goto err;
    }

  C2F(dgebak)("B", "R", &n, &ilo, &ihi, work, &n, (*D)->R, &n, &info, 1L, 1L);
  if (info != 0) 
    {
      Scierror("Error: something wrong in dgebak\n");
      goto err;
    }

  FREE(work);
  return OK;

 err:
  nsp_matrix_destroy(*D); 
  FREE(work);
  return FAIL;
}

static int intzgebal(NspMatrix *A,NspMatrix **D)
{
  int n = A->n, info;
  double *work ; 
  int ilo, ihi;

  if ( (*D=nsp_mat_eye(n,n)) == NULLMAT ) return FAIL;
  if ( (work = nsp_alloc_work_doubles(n)) == NULL ) goto err;

  C2F(zgebal)("B", &n, A->C, &n, &ilo, &ihi, work, &info, 1L);
  if (info != 0) 
    {
      Scierror("Error: something wrong in zgebal\n");
      goto err;
    }

  C2F(dgebak)("B", "R", &n, &ilo, &ihi, work, &n, (*D)->R, &n, &info, 1L, 1L);
  if (info != 0) 
    {
      Scierror("Error: something wrong in dgebak\n");
      goto err;
    }

  FREE(work);
  return OK;

 err:
  nsp_matrix_destroy(*D); 
  FREE(work);
  return FAIL;
}


static int intdggbal(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y);
static int intzggbal(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y);

/**
 * nsp_gbalanc:
 * @A: a #NspMatrix 
 * @B: a #NspMatrix 
 * @X: a pointer to a #NspMatrix 
 * @Y: a pointer to  #NspMatrix 
 * 
 * balances  a  pair  of matrices (@A,@B).
 * Balancing may reduce the 1-norm of  the  matrices,  and  improve  the
 * accuracy  of the computed eigenvalues and/or eigenvectors in the gen-
 * eralized eigenvalue problem A*x = lambda*B*x.
 * The returned matrices @X and @Y are such that Ab=X'*A*Y, Bb=X'*B*Y
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_gbalanc(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y)
{

  if (A->m != A->n) 
    { 
      Scierror("Error: first argument of balanc should be square and it is (%dx%d)\n", 
	       A->m,A->n);
      return FAIL;
    }

  if (B->m != B->n || B->m != A->m) 
    { 
      Scierror("Error: first and second arguments of balanc must have equal size\n");
      return FAIL;
    }
  /* 
   *  if (( B =nsp_matrix_copy(B) )== NULLMAT) return FAIL;
   * if (( A =nsp_matrix_copy(A) )== NULLMAT) return FAIL;
   */

  if ( A->mn == 0 )   /* A = [] return empty matrices */ 
    {
      if ( X != NULL)
	{
	  if (( *X =nsp_matrix_create(NVOID,A->rc_type,0,0)) == NULLMAT) return FAIL;
	}
      if ( Y != NULL)
	{
	  if (( *Y =nsp_matrix_create(NVOID,A->rc_type,0,0)) == NULLMAT) return FAIL;
	}
      return OK ; 
    }

  if ( nsp_mat_have_nan_or_inf(A) )
    {
      Scierror("Error: nan or inf in balanc first argument\n"); 
      return FAIL;
    }

  if ( nsp_mat_have_nan_or_inf(B) )
    {
      Scierror("Error: nan or inf in balanc first argument\n"); 
      return FAIL;
    }

  if ( A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r') 
	{
	  /* A real, b real */
	  if ( intdggbal(A,B,X,Y) == FAIL) return FAIL; 
	}
      else 
	{
	  /* A real, b complex */
	  if (nsp_mat_complexify(A,0.0) == FAIL ) return FAIL;
	  if ( intzggbal(A,B,X,Y) == FAIL) return FAIL; 
	}
    } 
  else
    {
      if ( B->rc_type == 'r') 
	{
	  /* A complex, B real */
	  if (nsp_mat_complexify(B,0.0) == FAIL ) return FAIL;
	  if ( intzggbal(A,B,X,Y) == FAIL) return FAIL; 
	}
      else 
	{
	  /* A complex, b complex */
	  if ( intzggbal(A,B,X,Y) == FAIL) return FAIL; 
	}
    }
  return OK;
}

/*  A revoir : A et B peuvent etre rectangulaires dimension (LDA,N)
 *  dimension dimension (LDB,N) mais LDB et LDA doivent etre >= N. 
 */ 

static int intdggbal(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y)
{
  double *lscale=NULL,*rscale=NULL,*dwork=NULL;
  NspMatrix *XX=NULLMAT, *YY=NULLMAT;
  int info,lworkMin,ilo,ihi;  
  int n = A->n;
  
  if ( (lscale=nsp_alloc_work_doubles(n)) == NULL ) return FAIL;
  if ( (rscale=nsp_alloc_work_doubles(n)) == NULL ) goto err;
  lworkMin = Max(1,6*n);
  if ( (dwork=nsp_alloc_work_doubles(lworkMin)) == NULL ) goto err;

  C2F(dggbal)("B",&n,A->R, &n,B->R, &n,&ilo,&ihi, lscale, rscale, dwork, &info, 1L);
  if (info != 0) 
    {
      Scierror("Error: something wrong in dggbal\n");
      goto err;
    }

  if ( X != NULL ) 
    { 
      if ( (XX=nsp_mat_eye(n,n)) == NULLMAT ) goto err;
      C2F(dggbak)("B", "L", &n, &ilo, &ihi, lscale, rscale, &n, XX->R, &n, &info, 1L,1L);
      if (info != 0) 
	{
	  Scierror("Error: something wrong in dggbak\n");
	  goto err;
	}
    }

  if ( Y != NULL ) 
    {
      if ( (YY=nsp_mat_eye(n,n)) == NULLMAT ) goto err;
      C2F(dggbak)("B", "R", &n, &ilo, &ihi, lscale, rscale, &n, YY->R, &n, &info, 1L, 1L);
      if (info != 0) 
	{
	  Scierror("Error: something wrong in dggbak\n");
	  goto err;
	}
    }

  FREE(lscale); FREE(rscale); FREE(dwork);
  if ( X != NULL ) *X = XX;
  if ( Y != NULL ) *Y = YY;
  return OK ; 

 err:
  FREE(lscale); FREE(rscale); FREE(dwork);
  nsp_matrix_destroy(XX); nsp_matrix_destroy(YY);
  return FAIL;
} 

static int intzggbal(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y)
{
  double *lscale=NULL,*rscale=NULL,*dwork=NULL;
  NspMatrix *XX=NULLMAT, *YY=NULLMAT;
  int info,lworkMin,ilo,ihi;  
  int n = A->n;

  if ( (lscale=nsp_alloc_work_doubles(n)) == NULL ) return FAIL;
  if ( (rscale=nsp_alloc_work_doubles(n)) == NULL ) goto err;
  lworkMin = Max(1,6*n);
  if ( (dwork=nsp_alloc_work_doubles(lworkMin)) == NULL ) goto err;

  C2F(zggbal)("B",&n, A->C, &n, B->C, &n, &ilo, &ihi, lscale, rscale, dwork, &info, 1L);
  if (info != 0) 
    {
      Scierror("Error: something wrong in zggbal\n");
      goto err;
    }

  if ( X != NULL ) 
    { 
      if ( (XX=nsp_mat_eye(n,n)) == NULLMAT ) goto err;
      C2F(dggbak)("B", "L", &n, &ilo, &ihi, lscale, rscale, &n, XX->R, &n, &info, 1L, 1L);
      if (info != 0) 
	{
	  Scierror("Error: something wrong in dggbak\n");
	  goto err;
	}
    }

  if ( Y != NULL ) 
    {
      if ( (YY=nsp_mat_eye(n,n)) == NULLMAT ) goto err;
      C2F(dggbak)("B", "R", &n, &ilo, &ihi, lscale, rscale, &n, YY->R, &n, &info, 1L, 1L);
      if (info != 0) 
	{
	  Scierror("Error: something wrong in dggbak\n");
	  goto err;
	}
    }

  FREE(lscale); FREE(rscale); FREE(dwork);
  if ( X != NULL ) *X = XX;
  if ( Y != NULL ) *Y = YY;
  return OK ; 

 err:
  FREE(lscale); FREE(rscale); FREE(dwork);
  nsp_matrix_destroy(XX); nsp_matrix_destroy(YY);
  return FAIL;
} 

static int intdgelsy(NspMatrix *A, NspMatrix *B, double rcond, int *rank);
static int intzgelsy(NspMatrix *A, NspMatrix *B, double rcond, int *rank);
/* static int intdgelsd(NspMatrix *A, NspMatrix *B, double rcond, int *rank); */
/* static int intzgelsd(NspMatrix *A, NspMatrix *B, double rcond, int *rank); */

/**
 * nsp_lsq:
 * @A: (input) a real or complex matrix (of the least square problem). @A is modified.
 * @B: (input/output) a real or complex vector or matrix (the rhs of the least square problem).
 *     On output @B holds the solution.
 * @Rcond: (input) if not NULL the tolerance for rank estimation. If NULL the tolerance is Max(m,n)eps
 * @Rank: (output) estimated rank 
 * 
 *  Solves a least square problem :
 *
 *
 *   Min || Ax - B ||^2
 *      
 *
 * When the matrix @A is not detected as full rank (which can depend an the tolerance @Rcond)
 * the minimum norm solution is computed.
 * 
 * FIXME :  an option to solve with svd could be interesting
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_lsq(NspMatrix *A, NspMatrix *B, double *Rcond, int *Rank)
{
  double rcond;

  if (Rcond == NULL ) 
    rcond = Max(A->m,A->n)*nsp_dlamch("eps");
  else 
    rcond = *Rcond;
  
  if ( A->m != B->m )
    {
      Scierror("Error: incompatible row dimensions in lsq\n");
      return FAIL;
    }

  /* test inf eand nan */
  if ( nsp_mat_have_nan_or_inf(A) || nsp_mat_have_nan_or_inf(B) )
    {
      Scierror("Error: nan or inf detected in matrix or rhs\n");
      return FAIL;
    }


  if ( A->rc_type == 'c' ) 
    {
      if ( B->rc_type == 'r' ) 
	{
	  if (nsp_mat_set_ival(B,0.00) == FAIL ) return FAIL;
	}
    }
  else 
    { 
      if ( B->rc_type == 'c' ) 
	{
	  if (nsp_mat_set_ival(A,0.00) == FAIL ) return FAIL;
	}
    }

  if ( A->rc_type == 'r')
    return intdgelsy(A, B, rcond, Rank);
  else
    return intzgelsy(A, B, rcond, Rank);
}


static int intdgelsy(NspMatrix *A, NspMatrix *B, double rcond, int *rank)
{
  int mA = A->m, nA = A->n, mB = B->m, nB = B->n; /* mA must be equal to mB */
  int mx = A->n, nx = B->n, ldB, ix, iB, j;
  int *jpvt = NULL, info, lwork;
  double qrwork[1], *rwork = NULL;

  if ( A->m == 0 || A->n == 0 || B->m == 0 || B->n ==0 ) 
    {
      if ((nsp_matrix_resize(B,mx,nx)) ==  FAIL) return FAIL;
      for ( j= 0 ; j < B->mn; j++) B->R[j]=0.0;
      return OK;
    }

  if ( mx > mB )
    /* enlarge B so that it can contains the solution x (needed by lapack) */
    {
      iB = B->mn-1;
      if ((nsp_matrix_resize(B,mx,nx)) ==  FAIL) return FAIL;
      for ( j = nB-1 ; j > 0 ; j--)   /* the first column is already good (so j>0) */
	for ( ix = j*mx+mB-1 ; ix >= j*mx ; ix--, iB-- )
	  B->R[ix] = B->R[iB];
      ldB = mx;
    }
  else
    ldB = mB;

  
  if ( (jpvt = nsp_alloc_work_int(nA)) == NULL )
    goto err;
  else
    for ( j = 0 ; j < nA ; j++ ) jpvt[j] = 0;
       
  lwork = -1;  /* query work size */
  C2F(dgelsy)(&mA, &nA, &nB, A->R, &mA, B->R, &ldB, jpvt, &rcond, rank, qrwork, &lwork, &info);
  
  if (info != 0) 
    {
      goto err;   /* message for info < 0 is given by xerbla.c  */
    } 


  lwork = (int) qrwork[0];
  if ( (rwork = nsp_alloc_work_doubles(lwork)) == NULL ) goto err;
  C2F(dgelsy)(&mA, &nA, &nB, A->R, &mA, B->R, &ldB, jpvt, &rcond, rank, rwork, &lwork, &info);

  if (info != 0) 
    {
      goto err;   /* message for info < 0 is given by xerbla.c  */
    } 
  
  if ( mx < mB )   /* free a part of B which is not needed */
    {
      /* first compress B (the solution x is in B but with a "leading"
       * dimension mB  => transform with a "leading" dimension mx ...)
       */
      ix = mx;
      for ( j = 1 ; j < nB ; j++)   /* the first column is already good */
	for ( iB = j*mB ; iB < j*mB+mx ; iB++, ix++)
	  B->R[ix] = B->R[iB];
      /* now we can free the part of B which is not used anymore */
      if (  nsp_matrix_resize(B, mx, nx) == FAIL )
	goto err;
    }

  FREE(jpvt); FREE(rwork);
  return OK;

 err:
  FREE(jpvt); FREE(rwork);
  return FAIL;
}


static int intzgelsy(NspMatrix *A, NspMatrix *B, double rcond, int *rank)
{
  int mA = A->m, nA = A->n, mB = B->m, nB = B->n; /* mA must be equal to mB */
  int mx = A->n, nx = B->n, ldB, ix, iB, j;
  int *jpvt = NULL, info, lwork;
  doubleC qcwork[1], *cwork = NULL;
  double *rwork = NULL;

  if ( A->m == 0 || A->n == 0 || B->m == 0 || B->n ==0 ) 
    {
      if ((nsp_matrix_resize(B,mx,nx)) ==  FAIL) return FAIL;
      for ( j= 0 ; j < B->mn; j++) B->R[j]=0.0;
      return OK;
    }

  if ( mx > mB )
    /* enlarge B so that it can contains the solution x (needed by lapack) */
    {
      iB = B->mn-1;
      if ((nsp_matrix_resize(B,mx,nx)) ==  FAIL) return FAIL;
      for ( j = nB-1 ; j > 0 ; j--)   /* the first column is already good (so j>0) */
	for ( ix = j*mx+mB-1 ; ix >= j*mx ; ix--, iB-- )
	  B->C[ix] = B->C[iB];
      ldB = mx;
    }
  else
    ldB = mB;

  if ( (jpvt = nsp_alloc_work_int(nA)) == NULL )
    goto err;
  else
    for ( j = 0 ; j < nA ; j++ ) jpvt[j] = 0;
       
  lwork = -1;  /* query work size */
  C2F(zgelsy)(&mA, &nA, &nB, A->C, &mA, B->C, &ldB, jpvt, &rcond, rank, qcwork, &lwork, rwork, &info);
  lwork = (int) qcwork[0].r;
  rwork = nsp_alloc_work_doubles(2*nA);
  cwork = nsp_alloc_work_doubleC(lwork);
  if ( (rwork == NULL) || (cwork==NULL) ) goto err;
  C2F(zgelsy)(&mA, &nA, &nB, A->C, &mA, B->C, &ldB, jpvt, &rcond, rank, cwork, &lwork, rwork, &info);
  
  if ( mx < mB )   /* free a part of B which is not needed */
    {
      /* first compress B (the solution x is in B but with a "leading"
       * dimension mB  => transform with a "leading" dimension mx ...)
       */
      ix = mx;
      for ( j = 1 ; j < nB ; j++)   /* the first column is already good */
	for ( iB = j*mB ; iB < j*mB+mx ; iB++, ix++)
	  B->C[ix] = B->C[iB];
      /* now we can free the part of B which is not used anymore */
      if (  nsp_matrix_resize(B, mx, nx) == FAIL )
	goto err;
    }

  FREE(jpvt); FREE(rwork); FREE(cwork);
  return OK;

 err:
  FREE(jpvt); FREE(rwork); FREE(cwork);
  return FAIL;
}


#if 0 
/* unused */

static int intdgesv4(NspMatrix *A,NspMatrix *B,NspMatrix **Sol,NspMatrix **rank,double *rcond,char flag);
static int intzgesv4(NspMatrix *A,NspMatrix *B,NspMatrix **Sol,NspMatrix **rank,double *rcond,char flag);

int nsp_slash(NspMatrix *A,NspMatrix *B,NspMatrix **Sol,NspMatrix **rank,double *rcond,char flag)
{
  if ( A->rc_type == 'r' ) 
    return  intdgesv4(A,B,Sol,rank,rcond,flag); 
  else 
    return  intzgesv4(A,B,Sol,rank,rcond,flag); 
}

static int intdgesv4(NspMatrix *A,NspMatrix *B,NspMatrix **Sol,NspMatrix **rank,double *rcond,char flag)
{
  NspMatrix *At,*Bt;
  if ((At =nsp_matrix_transpose(A)) == NULLMAT)  return FAIL;
  if ((Bt =nsp_matrix_transpose(B)) == NULLMAT)  return FAIL;
  if ( intdgesv3(At,Bt,rank,rcond,flag) == FAIL) return FAIL; 
  if (( *Sol =nsp_matrix_transpose(Bt)) == NULLMAT)  return FAIL;

  nsp_matrix_destroy(At);
  nsp_matrix_destroy(Bt);
  return OK;
}

static int intzgesv4(NspMatrix *A,NspMatrix *B,NspMatrix **Sol,NspMatrix **rank,double *rcond,char flag)
{
  NspMatrix *At,*Bt;
  if ((At =nsp_matrix_transpose(A)) == NULLMAT)  return FAIL;
  if ((Bt =nsp_matrix_transpose(B)) == NULLMAT)  return FAIL;
  if ( intzgesv3(At,Bt,rank,rcond,flag) == FAIL) return FAIL; 
  if (( *Sol =nsp_matrix_transpose(Bt)) == NULLMAT)  return FAIL;

  nsp_matrix_destroy(At);
  nsp_matrix_destroy(Bt);
  return OK;
}
#endif 

/* interface for schur */

int nsp_dgees0(NspMatrix *A,NspMatrix **U,int (*F)(const double *re,const double *im), NspMatrix **Sdim) 
{
  NspMatrix *wr=NULL,*wi=NULL,*dwork=NULL,*iwork=NULL;
  char jobN[]="N", *job= jobN;
  double *Uval=NULL, dwork_opt;
  int info,lworkMin=-1,sdim; 
  int m = A->m, n = A->n, *Iwork = NULL;
  char *sort = "N";

  /* to facilitate cleaning in case of bug */

  if ( U != NULL) *U=NULL;
  if ( Sdim != NULL) *Sdim=NULL;
  
  /*  A = [] return empty matrices */ 

  if ( A->mn == 0 ) 
    {
      if ( U != NULL) {
	if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) goto fail;
      }
      if ( Sdim != NULL) {
	if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) goto fail;
	(*Sdim)->R[0] = 0;
      }
      return OK ; 
    }
  
  if (m != n) { 
    Scierror("Error: first argument of schur should be square and it is (%dx%d)\n", 
	     m,n);
    goto fail;
  }

  if (( wr =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) goto fail;
  if (( wi =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) goto fail;

  if ( F != NULL ) 
    {
      if (( iwork =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) goto fail;   
      Iwork = (int *) iwork->R;
      sort = "S";
    }

  if ( U != NULL ) 
    {
      job = "V";
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) goto fail;   
      Uval = (*U)->R;      
    }

  /* get work size */

  C2F(dgees)(job,sort, F, &n,A->R,&n, &sdim,wr->R, wi->R,Uval,
	     &n, &dwork_opt, &lworkMin, Iwork, &info, 4L, 4L);

  lworkMin = dwork_opt;
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) goto fail;   

  
  C2F(dgees)(job,sort, F, &n,A->R,&n, &sdim,wr->R, wi->R,Uval,
	     &n, dwork->R, &lworkMin, Iwork, &info, 4L, 4L);

  if ( Sdim != NULL ) {
    if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) goto fail;   
    (*Sdim)->R[0] = sdim;
  }

  if (info > 0) {
    if (info <= n) 
      Scierror("Error: in schur the QR algorithm failed to compute all the eigenvalues;\n");
    else if (info == n + 1) 
      Scierror("Error: in schur eigenvalues could not be reordered (the problem is very ill-conditioned)\n");
    else if (info == n + 2) {
      Scierror("Error: in schur roundoff errors make leading eigenvalues no longer satisfy criterion\n");
    }
    goto fail;
  }

  if ( wr != NULL) nsp_matrix_destroy(wr); 
  if ( wi != NULL) nsp_matrix_destroy(wi); 
  if ( iwork != NULL) nsp_matrix_destroy(iwork); 
  if ( dwork != NULL) nsp_matrix_destroy(dwork); 

  return OK ;
 fail:
  if ( U != NULL) nsp_matrix_destroy(*U);
  if ( Sdim != NULL) nsp_matrix_destroy(*Sdim);
  if ( wr != NULL) nsp_matrix_destroy(wr); 
  if ( wi != NULL) nsp_matrix_destroy(wi); 
  if ( iwork != NULL) nsp_matrix_destroy(iwork); 
  if ( dwork != NULL) nsp_matrix_destroy(dwork); 
  return FAIL;
}

/* 
 * complex case 
 */

int nsp_zgees0(NspMatrix *A,NspMatrix **U,int (*F)(const doubleC *w), NspMatrix **Sdim) 
{
  char jobN[]="N", *job= jobN;
  doubleC *Uval=NULL, dwork_opt;
  NspMatrix *W=NULLMAT,*dwork=NULLMAT,*rwork=NULLMAT,*iwork=NULLMAT;
  int info,lworkMin,sdim, *Iwork=NULL; 
  int m = A->m, n = A->n;
  char *sort = "N";

  /* to facilitate cleaning in case of bug */

  if ( U != NULL) *U=NULL;
  if ( Sdim != NULL) *Sdim=NULL;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ( U != NULL) {
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) goto fail;   
    }
    if ( Sdim != NULL) {
      if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) goto fail;   
      (*Sdim)->R[0] = 0;
    }
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of schur should be square and it is (%dx%d)\n", m,n);
    goto fail;
  }

  if (( W =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT) goto fail;
  
  if (( rwork =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) goto fail;
  
  if ( F != NULL ) 
    {
      if (( iwork =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) goto fail;   
      sort = "S";
      Iwork = iwork->I;
    }

  if ( U != NULL ) 
    {
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) goto fail;   
      Uval = (*U)->C;
      job = "V";
    }
  
  lworkMin = -1; 
  C2F(zgees)(job,sort, F, &n,A->C,&n, &sdim,W->C,Uval,
	     &n, &dwork_opt, &lworkMin,rwork->R, Iwork, &info, 4L, 4L);
  if ( info == 0 ) 
    lworkMin = Max(dwork_opt.r,2*n);
  else 
    lworkMin = Max(1,2*n);
  
  if (( dwork =nsp_matrix_create(NVOID,'c',1,lworkMin)) == NULLMAT) goto fail;   

  C2F(zgees)(job,sort, F, &n,A->C,&n, &sdim,W->C,Uval,
	     &n, dwork->C, &lworkMin,rwork->R, Iwork, &info, 4L, 4L);

  if ( Sdim != NULL ) {
    if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) goto fail;   
    (*Sdim)->R[0] = sdim;
  }
  
  if (info > 0 ) {
    if ( info <= n ) 
      Scierror("Error: in schur, the QR algorithm failed to compute all the eigenvalues;\n");
    else if ( info == n+1) 
      {
	Scierror("Error: in schur, the eigenvalues could not be reordered\n");
	Scierror("\tbecause some eigenvalues were too close to separate \n");
	Scierror("\t(the problem  is  very  ill-conditioned);\n");
      }
    else if ( info == n+2) 
      {
	Scierror("Error: in schur, after reordering, roundoff changed values of some complex\n");
	
        Scierror("\teigenvalues so that leading eigenvalues in the Schur form no longer satisfy\n");
	
	Scierror("\tSELECT = .TRUE.. This could also be caused by underflow due to scaling.\n");
      }
    goto fail;
  } 
  else if ( info < 0 )
    {
      Scierror("Error: in schur the %d-th argument had an illegal value.\n",-info);
      goto fail;
    }

  /* menage */ 

  if ( W != NULL) nsp_matrix_destroy(W); 
  if ( iwork != NULL) nsp_matrix_destroy(iwork); 
  if ( rwork != NULL) nsp_matrix_destroy(rwork); 
  if ( dwork != NULL) nsp_matrix_destroy(dwork); 

  return OK ;

 fail:
  if ( U != NULL) nsp_matrix_destroy(*U);
  if ( Sdim != NULL) nsp_matrix_destroy(*Sdim);
  if ( W != NULL) nsp_matrix_destroy(W); 
  if ( iwork != NULL) nsp_matrix_destroy(iwork); 
  if ( rwork != NULL) nsp_matrix_destroy(rwork); 
  if ( dwork != NULL) nsp_matrix_destroy(dwork); 
  return FAIL;
}


/* qz algorithm, schur for two matrices */

int nsp_dgges(NspMatrix *A,NspMatrix *B,
	     int (*F)(const double *alphar,const double *alphai,const double *beta),
		    NspMatrix **VSL,NspMatrix **VSR,NspMatrix **Sdim) 
{
  double  *Vsl=NULL,*Vsr=NULL, workopt;
  NspMatrix *alphar,*alphai,*beta,*dwork,*iwork=NULLMAT;
  int info,lworkMin,sdim=0;
  int m = A->m, n = A->n, mb = B->m,nb = B->n ;  
  char *sort = "N",*jobvsl="N",*jobvsr= "N";

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ( VSR != NULL) {
      if (( *VSR =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
    }
    if ( VSL != NULL) {
      if (( *VSL =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
    }
    if ( Sdim != NULL) {
      if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;   
      (*Sdim)->R[0] = 0;
    }
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of gschur should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  if (mb != nb) { 
    Scierror("Error: second argument of gschur should be square and it is (%dx%d)\n", 
	     mb,nb);
    return FAIL;
  }

  if (m != mb || n != nb ) {
    Scierror("Error: gschur, first and second arguments must have equal size\n");
    return FAIL;
  }

  if (( alphar =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;
  if (( alphai =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;
  if (( beta   =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;
  
  if ( F != NULL ) 
    {
      if (( iwork =nsp_matrix_create(NVOID,'r',2*n,1)) == NULLMAT) return FAIL;   
      sort = "S";
    }

  if ( VSL != NULL ) 
    {
      if (( *VSL =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;  
      Vsl = (*VSL)->R;
      jobvsl = "V"; 
    }

  if ( VSR != NULL ) 
    {
      if (( *VSR =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;  
      Vsr = (*VSR)->R;
      jobvsr = "V"; 
    }
  
  lworkMin = -1;

  C2F(dgges)(jobvsl, jobvsr, sort,F, &n, A->R, &n, B->R, &n, 
	     &sdim,alphar->R, alphai->R, beta->R,Vsl, &n,Vsr, &n,
	     &workopt, &lworkMin, (F!=NULL) ? iwork->I: NULL, &info, 1L, 1L, 1L);
  
  if ( info != 0) workopt = 1;
  lworkMin = Max(8*(n)+16,workopt);
  
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;   

  C2F(dgges)(jobvsl, jobvsr, sort,F, &n, A->R, &n, B->R, &n, 
	     &sdim,alphar->R, alphai->R, beta->R,Vsl, &n,Vsr, &n,
	     dwork->R, &lworkMin, (F!=NULL) ? iwork->I: NULL, &info, 1L, 1L, 1L);
  
  if ( Sdim != NULL ) {
    if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;   
    (*Sdim)->R[0] = sdim;
  }
  
  if (info > 0)
    {
      if (info <= n) 
	Scierror("Error: schur, the QR algorithm failed to compute all the eigenvalues;\n");
      else if (info == n + 1) 
	Scierror("Error: schur, eigenvalues could not be reordered (the problem is very ill-conditioned)\n");
      else if (info == n + 2) 
	Scierror("Error: schur, roundoff errors make leading eigenvalues no longer satisfy criterion\n");
      else if (info == n + 3) 
	Scierror("Error: schur, reordering failed\n");
      return FAIL;
    }
  else if ( info < 0 ) 
    {
      Scierror("Error: qz, the %d-th argument had an illegal value\n",-info);
      return FAIL;
    }
  /* menage XXXX */ 
  return OK ;
}


int nsp_zgges(NspMatrix *A,NspMatrix *B,
	      int (*F)(const doubleC *alpha, const doubleC *beta),
	      NspMatrix **VSL,NspMatrix **VSR,NspMatrix **Sdim)
{
  doubleC  *Vsl=NULL,*Vsr=NULL, workopt;
  NspMatrix *alpha=NULLMAT,*beta=NULLMAT,*dwork=NULLMAT,*iwork=NULLMAT,*rwork=NULLMAT;
  int info,lworkMin,sdim;
  int m = A->m, n = A->n, mb = B->m,nb = B->n ;  
  char *sort = "N",*jobvsl="N",*jobvsr= "N";

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ( VSR != NULL) {
      if (( *VSR =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
    }
    if ( VSL != NULL) {
      if (( *VSL =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
    }
    if ( Sdim != NULL) {
      if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;   
      (*Sdim)->R[0] = 0;
    }
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of gschur should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  if (mb != nb) { 
    Scierror("Error: second argument of gschur should be square and it is (%dx%d)\n",mb,nb);
    return FAIL;
  }

  if (m != mb || n != nb ) {
    Scierror("Error: gschur, first and second arguments must have equal size\n");
    return FAIL;
  }

  if (( alpha =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT) return FAIL;
  if (( beta  =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT) return FAIL;
  if (( rwork =nsp_matrix_create(NVOID,'r',1,8*n)) == NULLMAT) return FAIL;   
  
  if ( F != NULL ) 
    {
      if (( iwork =nsp_matrix_create(NVOID,'r',2*n,1)) == NULLMAT) return FAIL;   
      sort = "S";
    }
  if ( VSL != NULL ) 
    {
      if (( *VSL =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;  
      Vsl = (*VSL)->C;
      jobvsl = "V"; 
    }

  if ( VSR != NULL ) 
    {
      if (( *VSR =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;  
      Vsr = (*VSR)->C;
      jobvsr = "V"; 
    }

  lworkMin=-1;


  C2F(zgges)(jobvsl, jobvsr, sort,F, &n, A->C, &n, B->C, &n, 
	     &sdim,alpha->C, beta->C,Vsl, &n,Vsr, &n,
	     &workopt, &lworkMin,rwork->R, (F!= NULL) ? iwork->I: NULL, &info, 1L, 1L, 1L);
  
  if ( info != 0) workopt.r = 1;
  lworkMin = Max(2*(n),workopt.r);
  
  if (( dwork =nsp_matrix_create(NVOID,'c',1,lworkMin)) == NULLMAT) return FAIL;   
  C2F(zgges)(jobvsl, jobvsr, sort,F, &n, A->C, &n, B->C, &n, 
	     &sdim,alpha->C, beta->C,Vsl, &n,Vsr, &n,
	     dwork->C, &lworkMin,rwork->R, (F!= NULL) ? iwork->I: NULL, &info, 1L, 1L, 1L);
  
  if ( Sdim != NULL ) {
    if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;   
    (*Sdim)->R[0] = sdim;
  }

  if (info > 0) 
    {
      if (info <= n) 
	Scierror("Error:the qz iteration failed to compute all the eigenvalues;\n");
      else if (info == n + 1) 
	Scierror("Error: qz, eigenvalues could not be reordered (the problem is very ill-conditioned)\n");
      else if (info == n + 2) 
	Scierror("Error: qz, roundoff errors make leading eigenvalues no longer satisfy criterion\n");
      else if (info == n + 3) 
	Scierror("Error: reordering failed in qz\n");
      return FAIL;
    }
  else if ( info < 0 ) 
    {
      Scierror("Error: qz, the %d-th argument had an illegal value\n",-info);
      return FAIL;
    }
  /* menage XXXX */ 
  return OK ;
}


/* OK 
 * [U,H]=hess(A) 
 * forme de hessenberg 
 *   H is returned in A 
 *   U is computed if requested 
 */

static int intzgehrd(NspMatrix *A, NspMatrix **U);
static int intdgehrd(NspMatrix *A, NspMatrix **U);

int nsp_hess(NspMatrix *A,NspMatrix **U) 
{
  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  
    {
      if ( U != NULL) 
	if ( (*U=nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT ) return FAIL;
      return OK ; 
    }

  if (A->m != A->n) 
    { 
      Scierror("Error: first argument of hess should be square and it is (%dx%d)\n", A->m,A->n);
      return FAIL;
    }

  if ( A->rc_type == 'r') 
    return intdgehrd(A,U);
  else 
    return intzgehrd(A,U);
}

static int intdgehrd(NspMatrix *A, NspMatrix **U)
{
  double *tau=NULL, *work=NULL, qwork[1];
  NspMatrix *u=NULLMAT;
  int lwork, info, n=A->n, un=1, sym_flag, i, k, kn;

  sym_flag = nsp_mat_is_symmetric(A);

  if ( (tau=nsp_alloc_work_doubles(n-1)) == NULL ) return FAIL;

  /* query work size */
  lwork = -1; C2F(dgehrd)(&n, &un, &n, A->R, &n, tau, qwork, &lwork, &info);
  lwork = (int) qwork[0];

  if ( (work=nsp_alloc_work_doubles(lwork)) == NULL ) goto err;

  C2F(dgehrd)(&n, &un, &n, A->R, &n, tau, work, &lwork, &info);

  if (info != 0)   /* this must not happen */
    {
      Scierror("Error: something wrong in dgehrd\n");
      goto err;
    }

  if ( U != NULL)  /* extract U */
    { 
      if ( (u=nsp_matrix_copy(A)) == NULLMAT ) goto err;
      C2F(dorghr)(&n, &un, &n, u->R, &n, tau, work, &lwork, &info);    
      if (info != 0)  /* this must not happen */
	{
	  Scierror("Error: something wrong in dorghr\n");
	  goto err;
	}
      *U = u;
    }

  /* extract H */ 
  nsp_mat_triu(A,-1);  
  if ( sym_flag )
    {
      nsp_mat_tril(A,1);
      /* force symetry */
      k = 1;
      for ( i = 0 ; i < n-1 ; i++ )
	{
	  kn = k + n-1;
	  A->R[k] = A->R[kn] = 0.5*(A->R[k] + A->R[kn]);
	  k = kn + 2;
	} 
    }
  FREE(tau); FREE(work);
  return OK;

 err:
  FREE(tau); FREE(work);
  nsp_matrix_destroy(u);
  return FAIL;
} 


static int intzgehrd(NspMatrix *A, NspMatrix **U)
{
  doubleC *tau=NULL, *work=NULL, qwork[1];
  NspMatrix *u=NULLMAT;
  int lwork, info, n=A->n, un=1, sym_flag;

  sym_flag = nsp_mat_is_symmetric(A);

  if ( (tau=nsp_alloc_work_doubleC(n-1)) == NULL ) return FAIL;

  /* query work size */
  lwork = -1; C2F(zgehrd)(&n, &un, &n, A->C, &n, tau, qwork, &lwork, &info);
  lwork = (int) qwork[0].r;

  if ( (work=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;

  C2F(zgehrd)(&n, &un, &n, A->C, &n, tau, work, &lwork, &info);

  if (info != 0)   /* this must not happen */
    {
      Scierror("Error: something wrong in zgehrd\n");
      goto err;
    }

  if ( U != NULL)  /* extract U */
    { 
      if ( (u=nsp_matrix_copy(A)) == NULLMAT ) goto err;
      C2F(zunghr)(&n, &un, &n, u->C, &n, tau, work, &lwork, &info);    
      if (info != 0)  /* this must not happen */
	{
	  Scierror("Error: something wrong in zunghr\n");
	  goto err;
	}
      *U = u;
    }

  /* extract H */ 
  nsp_mat_triu(A,-1);  
  if ( sym_flag ) nsp_mat_tril(A,1); /* FIXME : il faudrait peut être aussi forcer la symétrie */

  FREE(tau); FREE(work);
  return OK;

 err:
  FREE(tau); FREE(work);
  nsp_matrix_destroy(u);
  return FAIL;
} 




static double intzvnorm(NspMatrix *A, double p);
static double intdvnorm(NspMatrix *A, double p);

/**
 * nsp_vector_norm:
 * @A: a real or complex vector
 * @p: a #double which must be >= 1 
 * 
 * Computes the p-norm of the vector @A : 
 *
 *     ( sum_k |A_k|^p )^(1/p)
 * 
 * @p must be +Inf to compute the infinite norm ( max_k |A_k| )
 * @A is not modified
 * 
 * Return value: the p-norm of the vector @A or -1.0 in case of alloc
 *               problem (which may happen only for intzvnorm)
 **/

double nsp_vector_norm(NspMatrix *A, double p) 
{
  if ( A->mn == 0 ) return 0.0;  /*  A = [] return 0 */ 

  if ( A->rc_type == 'r' ) 
    return intdvnorm(A, p) ;
  else 
    return intzvnorm(A, p) ;
}

static double intdvnorm(NspMatrix *A, double p)
{
  int i;
  double norm=0.0, scale, xi_abs;

  if ( p == 1.0 )
    {
      i = 1;
      norm = C2F(dasum)(&(A->mn), A->R, &i);
    } 
  else if ( p == 2.0 )
    {
      i = 1;
      norm = C2F(dnrm2)(&(A->mn), A->R, &i);
    } 
  else
    {
      scale = fabs(A->R[0]);
      for ( i = 1 ; i < A->mn ; i++ )
	{
	  xi_abs = fabs(A->R[i]);
	  if ( xi_abs > scale ) scale = xi_abs;
	}

      if ( isinf(p) || scale == 0.0 )
	norm = scale;
      else 
	{
	  int nscale;
	  frexp(scale, &nscale); /* frac part (returned by frexp) is not used */
	  nscale = - nscale;
	  for ( i = 0 ; i < A->mn ; i++ )
	    norm += pow(ldexp(fabs(A->R[i]),nscale), p);
	  norm = ldexp(pow(norm,1.0/p), -nscale);
	}
    }
  return norm;
}

static double intzvnorm(NspMatrix *A, double p)
{
  int i;
  double norm=0.0, scale=0.0, *work=NULL;

  if ( p == 1.0 )
    {
      for ( i = 0 ; i < A->mn ; i++ )
	norm +=  nsp_abs_c(&(A->C[i]));
    }
  else if ( p == 2.0 )
    {
      i = 1;
      norm = C2F(dznrm2)(&(A->mn), A->C, &i);
    }
  else if ( isinf(p) )
    {
      double temp;
      for ( i = 0 ; i < A->mn ; i++ )
	if ( (temp = nsp_abs_c(&(A->C[i]))) > norm ) 
	  norm = temp;
    }
  else
    {
      if ( (work=nsp_alloc_work_doubles(A->mn)) == NULL ) return -1.0;

      for ( i = 0 ; i < A->mn ; i++ )
	{
	  work[i] = nsp_abs_c(&(A->C[i]));
	  if ( work[i] > scale ) scale = work[i];
	}

      if ( scale == 0.0 )
	norm = scale;
      else
	{
	  int nscale;
	  frexp(scale, &nscale); /* frac part (returned by frexp) is not used */
	  nscale = - nscale;
	  for ( i = 0 ; i < A->mn ; i++ )
	    norm += pow(ldexp(work[i],nscale), p);
	  norm = ldexp(pow(norm,1.0/p), -nscale);
	}
      
      FREE(work);
    }
  return norm;
}

  
static double intznorm(NspMatrix *A,char flag);
static double intdnorm(NspMatrix *A,char flag);
/**
 * nsp_matrix_norm:
 * @A: a real or complex matrix
 * @flag: character defining the kind of matrix norm to compute:
 *
 *        @flag='1' for 1-norm ||A||_1 = max of ||Ax||_1 for all x such that ||x||_1 = 1
 *        @flag='2' for 2-norm ||A||_2 = max of ||Ax||_2 for all x such that ||x||_2 = 1
 *        @flag='I' for Inf-norm ||A||_I = max of ||Ax||_inf for all x such that ||x||_inf = 1
 *        @flag='F' for Frobenius norm ||A||_F = sqrt( sum_{i,j} A(i,j)^2 )
 *        @flag='M' for  ||A||_F = max_{i,j} |A(i,j)|  (which is not exactly a matrix-norm)
 *
 * @A is not modified.
 * 
 * Return value: the matrix norm or -1 in case of failure (alloc problem or problem in
 *               svd (2-norm))
 **/

double nsp_matrix_norm(NspMatrix *A,char flag) 
{
  if ( A->mn == 0 ) return 0.0;  /*  A = [] return 0 */ 

  if ( flag == '2' )   /* brute force method : compute the max of the singular values */
    {
      NspMatrix *S=NULLMAT;
      if ( nsp_svd(A, &S, NULL, NULL, 'r', NULL, NULL) == FAIL )  /* rmk: the flag 'r' is not used */
	return -1.0;  /* bad trick... */
      else
	{
	  double norm = S->R[0];
	  nsp_matrix_destroy(S);
	  return norm;
	}
    }

  if ( A->rc_type == 'r' ) 
    return intdnorm(A,flag);
  else 
    return intznorm(A,flag);
}

static double intdnorm(NspMatrix *A, char flag)
{
  double norm, *dwork;
  int m=A->m, n=A->n;

  if ( flag == 'I') 
    {
      if ( (dwork=nsp_alloc_work_doubles(m)) == NULL ) return -1.0;  /* bad trick... */
      norm = C2F(dlange)(&flag, &m, &n, A->R, &m, dwork, 1L);
      FREE(dwork);
    } 
  else 
    norm=  C2F(dlange)(&flag, &m, &n, A->R, &m, NULL , 1L);

  return norm;
} 

static double intznorm(NspMatrix *A,char flag)
{
  double norm, *dwork;
  int m=A->m, n=A->n ;

  if ( flag == 'I') 
    {
      if ( (dwork=nsp_alloc_work_doubles(m)) == NULL ) return -1.0;  /* bad trick... */
      norm = C2F(zlange)(&flag, &m, &n, A->C, &m, dwork, 1L);
      FREE(dwork);
    } 
  else 
    norm = C2F(zlange)(&flag, &m, &n, A->C, &m, NULL , 1L);

  return norm;
}

/**
 * nsp_expm:
 * @A: a real or complex square matrix
 * 
 * computes the exponential of the matrix @A.
 * @A is overwritten with the result.
 * 
 * Return value: %FAIl or %OK
 **/

int nsp_expm(NspMatrix *A)
{
  int m, sym_flag, ideg=6, *ipiv=NULL, lwork, iexph, ns, iflag;
  double *rwork=NULL, t=1;
  doubleC *cwork=NULL;

  if ( A->m != A->n )
    {
      Scierror("Error: matrix must be square (in nsp_expm)\n");
      return FAIL;
    }

  m = A->m;

  if ( (ipiv = nsp_alloc_work_int(m)) == NULL )
    return FAIL;

  lwork = 4*m*m + ideg + 1;
  if ( A->rc_type == 'r' ) 
    {
      if ( (rwork = nsp_alloc_work_doubles(lwork)) == NULL ) 
	goto err;
    }
  else
    {
      if ( (cwork = nsp_alloc_work_doubleC(lwork)) == NULL ) 
	goto err;
    }

  sym_flag = nsp_mat_is_symmetric(A);

  if ( A->rc_type == 'r' ) 
    {
      if ( sym_flag )
	C2F(dspadm)(&ideg, &m, &t, A->R, &m, rwork, &lwork, ipiv, &iexph, &ns, &iflag);
      else
	C2F(dgpadm)(&ideg, &m, &t, A->R, &m, rwork, &lwork, ipiv, &iexph, &ns, &iflag);
    }
  else
    {
      if ( sym_flag )
	C2F(zhpadm)(&ideg, &m, &t, A->C, &m, cwork, &lwork, ipiv, &iexph, &ns, &iflag);
      else
	C2F(zgpadm)(&ideg, &m, &t, A->C, &m, cwork, &lwork, ipiv, &iexph, &ns, &iflag);
    }

  if ( iflag != 0 )
    {
      Scierror("Error: failure in nsp_expm\n");
      goto err;
    }

  if ( A->rc_type == 'r' ) 
    memcpy(A->R,&(rwork[iexph-1]),m*m*sizeof(double));
  else
    memcpy(A->C,&(cwork[iexph-1]),m*m*sizeof(doubleC));

  FREE(ipiv);
  FREE(rwork);
  FREE(cwork);
  return OK;

 err:
  FREE(ipiv);
  FREE(rwork);
  FREE(cwork);
  return FAIL;
}

/**
 * nsp_matrix_bdiv:
 * @A: a #NspMatrix (not modified)
 * @B: a #NspMatrix (not modified)
 * @tol_rcond: a double 
 * 
 * returns @A\@B. 
 * 
 * Returns: %OK or %FAIL
 **/

static NspMatrix *nsp_make_bandmat_from_mat(NspMatrix *A, int kl, int ku, Boolean enlarge);
 
NspMatrix *nsp_matrix_bdiv(NspMatrix *A, NspMatrix *B, double tol_rcond)
{
  int info, stat;
  double rcond;
  NspMatrix *C,*Ac;
  if ( A->m != B->m ) 
    {
      /* FIXME: must we treat the scalar case ? 
       *        A\scalar => x = inv(A)*scalar or pinv(A)*scalar 
       */
      Scierror("Error:\tIncompatible dimensions (in bdiv)\n");
      return NULLMAT;
    }
  
  if ( A->mn == 0 || B->mn == 0 )  
    {
      /* the special case */ 
      if ((C =nsp_matrix_create(NVOID, 'r', A->n, B->n)) == NULLMAT )
	return NULLMAT;
      nsp_mat_set_rval(C,0.0);
      return C;
    }
  
  if ((C = nsp_matrix_copy(B)) == NULLMAT) return NULLMAT;
  
  if ( A->m == A->n ) 
    {
      int kl, ku;
      /* A is square */
      /* tests on the structure of A */
      nsp_mat_lower_and_upper_bandwidth(A, &kl, &ku); 

      if (  kl == 0  ||  ku == 0 )  /* triangular or diagonal cases */
	{
	  if ( kl > 0 )
	    stat = nsp_mat_bdiv_triangular(A, C, 'l', 'n', &info);
	  else if ( ku > 0 )
	    stat = nsp_mat_bdiv_triangular(A, C, 'u', 'n', &info);
	  else
	    stat = nsp_mat_bdiv_diagonal(A, C, &info);
	  
	  if ( stat == FAIL ) 
	    goto err;
	  else if ( info != 0 )  /* important note: in this case C has not been modified */
	    Sciprintf("Warning: matrix is singular => computing a least square solution\n");
	  else
	    return C;
	}

      else 
	{
	  if ( 5*(kl + ku) <= A->m )  /* band case (the factor (5) must be ajusted...) */
	    {
	      NspMatrix *AB;
	      if ( (AB = nsp_make_bandmat_from_mat(A, kl, ku, TRUE)) == NULLMAT ) goto err;
	      stat = nsp_mat_bdiv_banded(AB, kl, ku, C, &rcond, tol_rcond, &info, NULL);
	      nsp_matrix_destroy(AB);
	    }
	  else
	    {
	      NspMatrix *Ac ;
	      if ( (Ac = nsp_matrix_copy(A)) == NULLMAT ) goto err;
	      stat = nsp_mat_bdiv_square(Ac, C, &rcond, tol_rcond);
	      nsp_matrix_destroy(Ac);
	    }

	  if ( stat == FAIL )
	    goto err;
	  else if ( rcond <= tol_rcond )
	    {
	      Sciprintf("Warning: matrix is singular or badly conditionned (rcond = %g)\n",rcond);
	      Sciprintf("\t=> computing a least square solution\n",rcond);
	      /* note that here C has not been modified, we can switch to bdiv_lsq */
	    }
	  else
	    return C;
	} 
    }
  
  if ( (Ac = nsp_matrix_copy(A)) == NULLMAT ) return NULLMAT;
  stat = nsp_mat_bdiv_lsq(Ac,C, tol_rcond);
  nsp_matrix_destroy(Ac);
  if ( stat == FAIL ) goto err;
  return C;

 err:
  nsp_matrix_destroy(C);
  return NULLMAT;
}


/**
 * nsp_mat_bdiv_lsq:
 * @A: a #NspMatrix (modified)
 * @B: a #NspMatrix (modified)
 * @tol_rcond: a double 
 * 
 * implements @A\@B when @A is not square (or
 * when @A is square but badly conditionned or singular)
 * using a least square method (dgelsy or zgelsy)
 * The result is returned in @B. 
 * 
 * 
 * Returns: %OK or %FAIL 
 **/

int nsp_mat_bdiv_lsq(NspMatrix *A, NspMatrix *B, double tol_rcond)
{  
  /* Note : A->m must be equal to B->m but this is verified at the upper level */
  int mA = A->m, nA = A->n, stat, rank;

  /* test inf and nan */
  if ( nsp_mat_have_nan_or_inf(A) || nsp_mat_have_nan_or_inf(B) )
    {
      Scierror("Error: nan or inf detected in matrix or rhs\n");
      return FAIL;
    }

  if ( A->rc_type == 'c' ) 
    {
      if ( B->rc_type == 'r' ) 
	{
	  if (nsp_mat_set_ival(B,0.00) == FAIL ) return FAIL;
	}
    }
  else 
    { 
      if ( B->rc_type == 'c' ) 
	{
	  if (nsp_mat_set_ival(A,0.00) == FAIL ) return FAIL;
	}
    }

  if ( A->rc_type == 'r')
    stat = intdgelsy(A, B, tol_rcond, &rank);
  else
    stat = intzgelsy(A, B, tol_rcond, &rank);

  if ( stat == OK )
    if ( rank < Min(mA,nA) )
      Sciprintf("Warning: matrix is rank-deficient m=%d, n=%d, rank=%d \n", mA, nA, rank);

  return stat;
}

/**
 * nsp_mat_bdiv_square:
 * @A: a #NspMatrix (modified)
 * @B: a #NspMatrix (modified)
 * @rcond: a double pointer 
 * @tol_rcond: a double 
 * 
 * implements A\B when A is square. B is overwritten by the solution X. 
 * 
 * Returns: %OK or %FAIL 
 **/

int nsp_mat_bdiv_square(NspMatrix *A, NspMatrix *B, double *rcond, double tol_rcond)
{  
  int n=A->m, nrhs=B->n;  /* mA must be equal to nA */
  int *ipiv=NULL, *iwork=NULL, info;
  double anorm, *rwork=NULL;
  doubleC *cwork=NULL;

  /* rmk: - when B is complex while A is real, something better than complexify A can be done */
  if ( A->rc_type == 'c' ) 
    {
      if ( B->rc_type == 'r' ) 
	{
	  if ( nsp_mat_set_ival(B,0.00) == FAIL ) return FAIL;
	}
    }
  else 
    { 
      if ( B->rc_type == 'c' ) 
	{
	  if ( nsp_mat_set_ival(A,0.00) == FAIL ) return FAIL;
	}
    }

  if ( (ipiv = nsp_alloc_work_int(n)) == NULL ) return FAIL;
       
  if ( A->rc_type == 'r' )
    { 
      anorm = C2F(dlange) ("1", &n, &n, A->R, &n, NULL, 1);
      C2F(dgetrf) (&n, &n, A->R, &n, ipiv, &info);
      if ( info != 0 )  /* a pivot is exactly zero */
	{
	  *rcond = 0.0; FREE(ipiv); return OK;
	}
      if ( (iwork = nsp_alloc_work_int(n)) == NULL ) goto err;
      if ( (rwork = nsp_alloc_work_doubles(4*n)) == NULL ) goto err;      

      C2F(dgecon) ("1", &n, A->R, &n, &anorm, rcond, rwork, iwork, &info, 1);
      if ( *rcond <=  tol_rcond ) /* matrix is too badly conditionned */
	{
	  FREE(rwork); FREE(iwork); FREE(ipiv); return OK;
	}
      C2F(dgetrs) ("N", &n, &nrhs, A->R, &n, ipiv, B->R, &n, &info, 1);
      FREE(rwork); FREE(iwork); FREE(ipiv); return OK;
    }
  else
    {
      anorm = C2F(zlange) ("1", &n, &n, A->C, &n, NULL, 1);
      C2F(zgetrf) (&n, &n, A->C, &n, ipiv, &info);
      if ( info != 0 )  /* a pivot is exactly zero */
	{
	  *rcond = 0.0; FREE(ipiv); return OK;
	}
      if ( (rwork = nsp_alloc_work_doubles(2*n)) == NULL ) goto err;      
      if ( (cwork = nsp_alloc_work_doubleC(2*n)) == NULL ) goto err;

      C2F(zgecon) ("1", &n, A->C, &n, &anorm, rcond, cwork, rwork, &info, 1);
      if ( *rcond <= tol_rcond )  /* matrix is too badly conditionned */
	{
	  FREE(cwork); FREE(rwork); FREE(ipiv); return OK;
	}
      C2F(zgetrs) ("N", &n, &nrhs, A->C, &n, ipiv, B->C, &n, &info, 1);
      FREE(cwork); FREE(rwork); FREE(ipiv); return OK;
    }

 err:
  FREE(cwork); FREE(rwork); FREE(iwork); FREE(ipiv); 
  return FAIL;
}

/**
 * nsp_mat_bdiv_triangular:
 * @A: a #NspMatrix (not modified)
 * @B: a #NspMatrix (modified)
 * @tri_type: a char 'u' for upper triangular, 'l' for lower triangular
 * @trans: a char 'n', (solve Tx=B) 't' (solve T^t x=B), or 'c' (solve T^*x=B)
 * @info: an int 0 if all is OK else a zero pivot have been met.
 * 
 * solve a linear triangular system A X = B, B is overwritten by the solution X
 *
 * Return value: OK or FAIL (due to  malloc failure)
 **/

int nsp_mat_bdiv_triangular(NspMatrix *A, NspMatrix *B, char tri_type, char trans, int *info)
{  
  int m = A->m, mn, i;
  double *temp;

  if ( A->rc_type == 'r' )
    {
      if ( B->rc_type == 'r' )
	C2F(dtrtrs) (&tri_type, &trans, "N", &m, &(B->n), A->R, &m, B->R, &m, info, 1,1,1);
      else  /* B is complex */
	{
	  mn = m * B->n;
	  if ( (temp = nsp_alloc_work_doubles(mn)) == NULL ) return FAIL;
	  for ( i = 0 ; i < mn ; i++ ) temp[i] = B->C[i].r;
	  C2F(dtrtrs) (&tri_type, &trans, "N", &m, &(B->n), A->R, &m, temp, &m, info, 1,1,1);
	  if ( *info != 0 ) { FREE(temp); return FAIL;}
	  for ( i = 0 ; i < mn ; i++ )  { B->C[i].r = temp[i]; temp[i] = B->C[i].i;}
	  C2F(dtrtrs) (&tri_type, &trans, "N", &m, &(B->n), A->R, &m, temp, &m, info, 1,1,1);
	  for ( i = 0 ; i < mn ; i++ )  B->C[i].i = temp[i];
	  FREE(temp);
	}
    }
  else
    {
      if ( B->rc_type == 'r' ) 
	if (nsp_mat_set_ival(B,0.00) == FAIL ) return FAIL;
      C2F(ztrtrs) (&tri_type, &trans, "N", &m, &(B->n), A->C, &m, B->C, &m, info, 1,1,1);
    }
  return OK;
}

/**
 * nsp_mat_bdiv_diagonal:
 * @A: a #NspMatrix (not modified)
 * @B: a #NspMatrix (modified)
 * @info: an int 0 if all is OK else a zero pivot have been met.
 * 
 * solve a linear diagonal system A X = B, B is overwritten by the solution X
 * if info > 0, B has not been modified
 *
 * Return value: OK or FAIL (FAIL only due to  malloc failure)
 **/

int nsp_mat_bdiv_diagonal(NspMatrix *A, NspMatrix *B, int *info)
{  
  int m = A->m, i, j, kA, kB;
  doubleC res;

  *info = 0;
  if ( A->rc_type == 'r' )
    {
      /* first test for singularity */
      for ( i = 0, kA = 0 ; i < m ; i++, kA+=m+1 )
	if ( A->R[kA] == 0.0 ) 
	  { *info = i+1; return OK;}

      if ( B->rc_type == 'r' )
	for ( i = 0, kA = 0 ; i < m ; i++, kA+=m+1 )
	  for ( j = 0, kB = i; j < B->n; j++, kB+=m )
	    B->R[kB] /= A->R[kA];
      else
	for ( i = 0, kA = 0 ; i < m ; i++, kA+=m+1 )
	  for ( j = 0, kB = i; j < B->n; j++, kB+=m )
	    {
	      B->C[kB].r /= A->R[kA];
	      B->C[kB].i /= A->R[kA];
	    }
    }
  else
    {
      /* first test for singularity */
      for ( i = 0, kA = 0 ; i < m ; i++, kA+=m+1 )
	if ( A->C[kA].r == 0.0  &&  A->C[kA].i == 0.0 ) 
	  { *info = i+1; return OK;}

      if ( B->rc_type == 'r' ) 
	if (nsp_mat_set_ival(B,0.00) == FAIL ) return FAIL;
      for ( i = 0, kA = 0 ; i < m ; i++, kA+=m+1 )
	for ( j = 0, kB = i; j < B->n; j++, kB+=m )
	  {
	    nsp_div_cc(&(B->C[kB]),&(A->C[kA]),&res);
	    B->C[kB] = res;
      NSP_OBJECT(A)->ret_pos = 2;
	  }
    }
  return OK;
}

/* 
 *  estimate rcond1 using lapack estimator (real case) 
 *  replace dgbcon which tends to be very slow (far
 *  more slower than the factorization) when n is big
 *  enough. The drawback is that nsp_dgbcon is certainly
 *  less robust because no scaling is done.
 *  nsp_dgbcon has the same interface than dgbcon for
 *  easy possible change but some arguments are not used.
 */
static void nsp_dgbcon(char *s_rien, int *n, int *kl, int *ku, double *AAb, int *lda, int *ipiv, double *normA1, 
		       double *rcond, double *work, int *isign, int *ier, int irien)
{
  int kase, one=1;
  double *X=work, *V=&work[*n], est;

  if ( *normA1 == 0.0 ) { *rcond = 0.0; return; }

  /* compute an estimation of ||inv(A)||_1 using lapack dlacon  */
  kase = 0;
  do
    {
      C2F(dlacon)(n, V, X, isign, &est, &kase);

      switch (kase)
	{
	case 1:  /* solve Y = A^(-1) X <=> A Y = X  with X overwritten by Y */
	  C2F(dgbtrs) ("N", n, kl, ku, &one, AAb, lda, ipiv, X, n, ier, 1);
	  break;

	case 2: /* solve Y = A'^(-1) X <=> A' Y = X  with X overwritten by Y */
	  C2F(dgbtrs) ("T", n, kl, ku, &one, AAb, lda, ipiv, X, n, ier, 1);
	  break;
	}
    }
  while ( kase != 0 );

  *rcond = 1.0/((*normA1)*est);
}

/* 
 *  estimate rcond1 using lapack estimator (complex case) 
 *  replace zgbcon which tends to be very slow (far
 *  more slower than the factorization) when n is big
 *  enough. The drawback is that nsp_zgbcon is certainly
 *  less robust because no scaling is done.
 *  nsp_zgbcon has the same interface than zgbcon for
 *  easy possible change but some arguments are not used. 
 */
static void nsp_zgbcon(char *s_rien, int *n, int *kl, int *ku, doubleC *AAb, int *lda, int *ipiv, double *normA1, 
		       double *rcond, doubleC *cwork, double *work, int *ier, int irien)
{
  int kase, one=1;
  doubleC *X=cwork, *V=&cwork[*n];
  double est;

  if ( *normA1 == 0.0 ) { *rcond = 0.0; return; }

  /* compute an estimation of ||inv(A)||_1 using lapack dlacon  */
  kase = 0;
  do
    {
      C2F(zlacon)(n, V, X, &est, &kase);

      switch (kase)
	{
	case 1:  /* solve Y = A^(-1) X <=> A Y = X  with X overwritten by Y */
	  C2F(zgbtrs) ("N", n, kl, ku, &one, AAb, lda, ipiv, X, n, ier, 1);
	  break;

	case 2: /* solve Y = A'^(-1) X <=> A' Y = X  with X overwritten by Y */
	  C2F(zgbtrs) ("T", n, kl, ku, &one, AAb, lda, ipiv, X, n, ier, 1);
	  break;
	}
    }
  while ( kase != 0 );

  *rcond = 1.0/((*normA1)*est);
}

/* compute a rough estimate of rcond using min(abs(diag(U)))/max(abs(diag(U))) */
/* the diagonal of U correspond to the row number kl+ku of the matrix Ab */
static double nsp_rough_rcond_for_banded_mat(NspMatrix *Ab, int kl, int ku)
{
  int i, k;
  double umin, umax, ui;

  if ( Ab->rc_type == 'r' )
    {
      umin = umax = fabs(Ab->R[kl+ku]);
      for ( i = 1, k = kl+ku+Ab->m ; i < Ab->n ; i++, k+=Ab->m )
	{
	  ui = fabs(Ab->R[k]);
	  if ( ui < umin )
	    umin = ui;
	  else if ( ui > umax )
	    umax = ui;
	}
    }
  else  /* Ab is complex */
    {
      umin = umax = nsp_abs_c(Ab->C+kl+ku);
      for ( i = 1, k = kl+ku+Ab->m ; i < Ab->n ; i++, k+=Ab->m )
	{
	  ui = nsp_abs_c(Ab->C+k);
	  if ( ui < umin )
	    umin = ui;
	  else if ( ui > umax )
	    umax = ui;
	}
    }
  return umin/umax;
}

/**
 * nsp_mat_bdiv_banded:
 * @Ab: (input/output) a real or complex banded matrix (the lapack storage is used 
 *     but it is assumed that kl additionnal rows have been put on the top of the
 *     matrix for storage purpose). The matrix is overwritten with its factorization.
 * @kl: (input) the lower bandwidth of @Ab
 * @ku: (input) the upper bandwidth of @Ab
 * @B: (input/output) a real or complex vector or matrix. @B is the rhs and is
 *                    overwritten by the solution X
 * @rcond: (output) a real number the estimated reciprocal condition number
 * @tol_rcond: (input) real number, when it is > 0 and when rcond >= tol_rcond 
 *             the 2 triangular system are not solved
 * @info: (output)
 *
 * Solves a linear system Ab X = B, with @Ab a banded matrix.
 * 
 * Return value: %OK or %FAIL;
 **/

int nsp_mat_bdiv_banded(NspMatrix *Ab, int kl, int ku, NspMatrix *B, double *rcond, 
			double tol_rcond, int *info, double *rrcond)
{
  int n = Ab->n, *ipiv=NULL, *iwork=NULL, ier;
  double *work=NULL, normA1=0.0;
  doubleC *cwork=NULL;

  if (Ab->m != ku+1+kl+kl ) 
    { 
      Scierror("Error: number of rows should equal to kl+kl+1+ku=%d and it is %d\n",ku+1+kl+kl,Ab->m);
      return FAIL;
    }

  if (B->m != n) 
    { 
      Scierror("Error: second argument is incompatible with the matrix order\n");
      return FAIL;
    }

  /* step 0 compute matrix norm (before the matrix is overwritten by its factorization...) */
  if ( rcond != NULL )
    {
      if ( Ab->rc_type == 'r' )
	normA1 = C2F(dlangb)("1", &n, &kl, &ku, &(Ab->R[kl]), &(Ab->m), NULL, 1);
      else
	normA1 = C2F(zlangb)("1", &n, &kl, &ku, &(Ab->C[kl]), &(Ab->m), NULL, 1);
    }

  /* step 1 factorize */
  if ( (ipiv =nsp_alloc_work_int(n)) == NULL ) goto err;
  if ( Ab->rc_type == 'r' ) 
    C2F(dgbtrf)(&n, &n, &kl, &ku, Ab->R, &(Ab->m), ipiv, info);
  else 
    C2F(zgbtrf)(&n, &n, &kl, &ku, Ab->C, &(Ab->m), ipiv, info);

  if ( rrcond != NULL )  /* compute rought estimate of rcond using min(abs(diag(U)))/max(abs(diag(U))) */
    *rrcond = nsp_rough_rcond_for_banded_mat(Ab, kl, ku);


  if ( *info != 0 )
    {
      if ( rcond != NULL ) *rcond = 0.0;
      goto err_bis;
    }

  /* step 2 compute rcond */
  if ( rcond != NULL )
    {
      if ( Ab->rc_type == 'r' )
	{
	  if ( (iwork =nsp_alloc_work_int(n)) == NULL ) goto err;
	  if ( (work =nsp_alloc_work_doubles(3*n)) == NULL ) goto err;
/* 	  C2F(dgbcon)("1", &n, &kl, &ku, Ab->R, &(Ab->m), ipiv, &normA1, rcond, work, iwork, &ier,1); */
	  nsp_dgbcon("1", &n, &kl, &ku, Ab->R, &(Ab->m), ipiv, &normA1, rcond, work, iwork, &ier,1);
	  FREE(iwork); FREE(work);
	}
      else
	{
	  if ( (cwork =nsp_alloc_work_doubleC(2*n)) == NULL ) goto err;
/* 	  if ( (work =nsp_alloc_work_doubles(n)) == NULL ) goto err; */
/* 	  C2F(zgbcon)("1", &n, &kl, &ku, Ab->C, &(Ab->m), ipiv, &normA1, rcond, cwork, work, &ier,1); */
	  nsp_zgbcon("1", &n, &kl, &ku, Ab->C, &(Ab->m), ipiv, &normA1, rcond, cwork, NULL, &ier,1);
	  FREE(cwork); FREE(work);
	}

      if ( tol_rcond > 0.0  &&  *rcond <= tol_rcond )  /* matrix is too badly conditionned */
	{
	  *info = -1;
	  goto err_bis;
	}
    }

  /* step 3 compute solution solving the 2 triangular systems */
  if ( Ab->rc_type == 'r' )
    {
      if ( B->rc_type == 'r' )
	C2F(dgbtrs) ("N", &n, &kl, &ku, &B->n, Ab->R, &Ab->m, ipiv, B->R, &B->m, &ier, 1);
      else  
	{
	  double *xwork;
	  int k;
	  if ( (xwork = nsp_alloc_work_doubles(B->mn)) == NULL ) goto err;
	  for ( k = 0 ; k < B->mn ; k++ ) xwork[k] = B->C[k].r;
	  C2F(dgbtrs) ("N", &n, &kl, &ku, &B->n, Ab->R, &Ab->m, ipiv, xwork, &B->m, &ier, 1);
	  for ( k = 0 ; k < B->mn ; k++ ) { B->C[k].r = xwork[k];  xwork[k] =  B->C[k].i; }
	  C2F(dgbtrs) ("N", &n, &kl, &ku, &B->n, Ab->R, &Ab->m, ipiv, xwork, &B->m, &ier, 1);
	  for ( k = 0 ; k < B->mn ; k++ ) B->C[k].i = xwork[k];
	  FREE(xwork);
	}
    }
  else
    {
      if ( B->rc_type == 'r' )
	if (nsp_mat_set_ival(B,0.00) == FAIL ) goto err;
      C2F(zgbtrs) ("N", &n, &kl, &ku, &B->n, Ab->C, &Ab->m, ipiv, B->C, &B->m, &ier, 1);
    }

  FREE(ipiv);
  return OK;

 err:
  FREE(iwork); FREE(work); FREE(cwork); FREE(ipiv);
  return FAIL;

 err_bis:
  FREE(iwork); FREE(work); FREE(cwork); FREE(ipiv);
  return OK;
}
	      
/*  if enlarge is true, prepare the band matrix directly for linear solving 
 *  (that is add kl rows at the beginning for storage purpose and so avoid
 *   the additionnal call to nsp_increase_banded_mat) 
 */
static NspMatrix *nsp_make_bandmat_from_mat(NspMatrix *A, int kl, int ku, Boolean enlarge)
{
  NspMatrix *AA;
  int j, imin, imax, idest, size, stride;

  stride = enlarge ? kl : 0;
  
  if ( (AA = nsp_matrix_create(NVOID, A->rc_type, kl+1+ku+stride, A->n)) == NULLMAT )
    return NULLMAT;

  if (  A->rc_type == 'r' )
    for ( j = 0; j < A->n; j++ )
      {
	imin = Max(0, j-ku);
	idest = stride + Max(0, ku + imin - j);
	imax = Min(A->m, j+kl);
	size = imax - imin + 1;
	memcpy(&(AA->R[j*AA->m+idest]),&(A->R[j*A->m+imin]),size*sizeof(double));
      }
  else
    for ( j = 0; j < A->n; j++ )
      {
	imin = Max(0, j-ku);
	idest = stride + Max(0, ku + imin - j);
	imax = Min(A->m, j+kl);
	size = imax - imin + 1;
	memcpy(&(AA->C[j*AA->m+idest]),&(A->C[j*A->m+imin]),size*sizeof(doubleC));
      }
  return AA;
}

NspMatrix *nsp_increase_banded_mat(NspMatrix *A, int kl, char flag)
{
  NspMatrix *AA;
  int i, j, kA, kAA;
  if ( A->rc_type == 'r' )
    {
      if ( flag == 'r' )
	{
	  AA = nsp_matrix_create(NVOID, 'r', A->m+kl, A->n);
	  if ( AA )
	    for ( j = 0; j < A->n; j++ )
	      memcpy(&(AA->R[j*AA->m+kl]),&(A->R[j*A->m]),A->m*sizeof(double));
	}
      else /* flag = 'c' */
	{
	  AA = nsp_matrix_create(NVOID, 'c', A->m+kl, A->n);
	  if ( AA )
	    {
	      kA = 0; kAA = 0;
	      for ( j = 0; j < A->n; j++ )
		{
		  kAA += kl;
		  for ( i = 0; i < A->m; i++ )
		    {
		      AA->C[kAA].r = A->R[kA++];
		      AA->C[kAA++].i = 0.0;
		    }
		}
	    }
	}
    }
  else
    {
      AA = nsp_matrix_create(NVOID, 'c', A->m+kl, A->n);
      if ( AA )
	for ( j = 0; j < A->n; j++ )
	  memcpy(&(AA->C[j*AA->m+kl]),&(A->C[j*A->m]),A->m*sizeof(doubleC));
    }
  return AA;
}



/*  symmetric positive definite matrix 
 *  or hermitian positive definite matrix
 */

int nsp_mat_bdiv_square_pos_symmetric(NspMatrix *A, NspMatrix *B, double *rcond, double tol_rcond)
{
  int n=A->m, nrhs=B->n, rep = FAIL;  /* mA must be equal to nA */
  int *iwork=NULL, info;
  double anorm, *rwork=NULL;
  doubleC *cwork=NULL;

  /* rmk: - when B is complex while A is real, something better than complexify A can be done */
  if ( A->rc_type == 'c' ) 
    {
      if ( B->rc_type == 'r' ) 
	{
	  if ( nsp_mat_set_ival(B,0.00) == FAIL ) goto end;
	}
    }
  else 
    { 
      if ( B->rc_type == 'c' ) 
	{
	  if ( nsp_mat_set_ival(A,0.00) == FAIL ) goto end;
	}
    }
       
  if ( A->rc_type == 'r' )
    { 
      anorm = C2F(dlange) ("1", &n, &n, A->R, &n, NULL, 1);
      C2F(dpotrf)("U", &n,A->R, &n, &info, 1L);
      if (info != 0) 
	{
	  if (info > 0)
	    {
	      Scierror("Error: matrix is not positive definite (minor %d)\n",info); 
	      *rcond = 0.0; rep = FAIL; goto end;
	    }
	  Scierror("Eroor: the %d-th argument of [dz]potrf had an illegal value\n",-info);
	  goto end;
	}

      /* reciprocal of the condition number */
      if ( (iwork = nsp_alloc_work_int(n)) == NULL ) goto end;
      if ( (rwork = nsp_alloc_work_doubles(3*n)) == NULL ) goto end;      
      C2F(dpocon) ("U", &n, A->R, &n, &anorm, rcond, rwork, iwork, &info, 1);
      if ( *rcond <=  tol_rcond ) 
	{
	  rep = OK; goto end;
	}
      C2F(dpotrs)("U",&n, &nrhs, A->R,&n, B->R, &n, &info, 1);
      rep =OK ; goto end;
    }
  else
    {
      anorm = C2F(zlange) ("1", &n, &n, A->C, &n, NULL, 1);
      C2F(zpotrf)("U", &n,A->C, &n, &info, 1L);
      if (info != 0) 
	{
	  if (info > 0)
	    {
	      Scierror("Error: matrix is not positive definite (minor %d)\n",info); 
	      *rcond = 0.0; rep = FAIL; goto end;
	    }
	  Scierror("Eroor: the %d-th argument of [dz]potrf had an illegal value\n",-info);
	  goto end;
	}
      /* reciprocal of the condition number */
      if ( (rwork = nsp_alloc_work_doubles(n)) == NULL ) goto end;      
      if ( (cwork = nsp_alloc_work_doubleC(2*n)) == NULL ) goto end;
      C2F(zpocon) ("U", &n, A->C, &n, &anorm, rcond, cwork, rwork, &info, 1);
      if ( *rcond <= tol_rcond ) 
	{
	  rep =OK ; goto end;
	}
      *rcond =10* tol_rcond;
      C2F(zpotrs)("U",&n, &nrhs, A->C,&n, B->C, &n, &info, 1);
      rep =OK ; goto end;
    }

 end:
  FREE(cwork); FREE(rwork); FREE(iwork);
  return rep;
}

/*  Ok. Sinon lapack a tout ce qu'il faut pour du symétrique indéfini :
  dsytrf pour factoriser, dsycon pour rcond et dsytrs pour résoudre.
 *
 *
 */

int nsp_mat_bdiv_square_symmetric(NspMatrix *A, NspMatrix *B, double *rcond, double tol_rcond)
{
  int rep = FAIL, lwork;
  int n=A->m, nrhs=B->n;  /* mA must be equal to nA */
  int *ipiv=NULL, *iwork=NULL, info;
  double anorm, *rwork=NULL,q_work;
  doubleC *cwork=NULL;

  /* rmk: - when B is complex while A is real, something better than complexify A can be done */
  if ( A->rc_type == 'c' ) 
    {
      if ( B->rc_type == 'r' ) 
	{
	  if ( nsp_mat_set_ival(B,0.00) == FAIL ) return FAIL;
	}
    }
  else 
    { 
      if ( B->rc_type == 'c' ) 
	{
	  if ( nsp_mat_set_ival(A,0.00) == FAIL ) return FAIL;
	}
    }

  if ( (ipiv = nsp_alloc_work_int(n)) == NULL ) return FAIL;
       
  if ( A->rc_type == 'r' )
    { 
      anorm = C2F(dlange) ("1", &n, &n, A->R, &n, NULL, 1);
      lwork = -1;
      C2F(dsytrf) ("U", &n, A->R, &n,ipiv,&q_work,&lwork,&info,1);
      lwork =Max(2*n,q_work);
      if ( (rwork = nsp_alloc_work_doubles(lwork)) == NULL ) goto end;
      C2F(dsytrf) ("U", &n, A->R, &n,ipiv,rwork,&lwork,&info,1);
      if ( info > 0 )  
	{
	  *rcond = 0.0; FREE(ipiv); return OK;
	}
      if ( (iwork = nsp_alloc_work_int(n)) == NULL ) goto end;
      C2F(dsycon) ("U", &n, A->R, &n,ipiv, &anorm, rcond, rwork, iwork, &info, 1);
      if ( *rcond <=  tol_rcond ) /* matrix is too badly conditionned */
	{
	  rep = OK ; goto end ;
	}
      
      C2F(dsytrs) ("U", &n, &nrhs, A->R, &n, ipiv, B->R, &n, &info, 1);
      rep = OK; goto end;
    }
  else
    {
      doubleC q_cwork;
      anorm = C2F(zlange) ("1", &n, &n, A->C, &n, NULL, 1);
      lwork = -1;
      C2F(zsytrf) ("U", &n, A->C, &n,ipiv,&q_cwork,&lwork,&info,1);
      lwork = q_cwork.r;
      lwork =Max(2*n,lwork);
      if ( (cwork = nsp_alloc_work_doubleC(lwork)) == NULL ) goto end;
      C2F(zsytrf) ("U", &n, A->C, &n,ipiv,cwork,&lwork,&info,1);
      if ( info > 0 ) 
	{ 
	  *rcond = 0.0; FREE(ipiv); return OK;
	}
      C2F(zsycon) ("U", &n, A->C, &n,ipiv, &anorm, rcond, cwork, &info, 1);
      if ( *rcond <= tol_rcond )  /* matrix is too badly conditionned */
	{
	  rep = OK; goto end;
	}
      C2F(zsytrs) ("U", &n, &nrhs, A->C, &n, ipiv, B->C, &n, &info, 1);
      rep = OK ; goto end;
    }

 end: 
  FREE(cwork); FREE(rwork); FREE(iwork); FREE(ipiv); 
  return rep;

}

typedef double (*funFr)(double arg);
typedef void (*funFc)(const doubleC *arg, doubleC *val);

NspMatrix *nsp_matrix_funm(NspMatrix *A, funFr Fr, funFc Fc,char *name, int flag)
{
  int i,j;
  NspMatrix *B= NULL, *vp = NULL, *Res= NULL, *U=NULL;
  if ( A->m != A->n )
    {
      Scierror("Error: %s argument should be a square matrix\n",name); 
      return NULLMAT;
    }
  if ( ( B= nsp_matrix_copy(A)) == NULLMAT) return NULLMAT;
  if ( nsp_mat_is_symmetric(A)== TRUE ) 
    {
      if ( ( vp =nsp_matrix_create(NVOID,A->rc_type,A->m,1)) == NULLMAT ) goto err;
      if ( A->rc_type == 'r') 
	{
	  if ( nsp_dgees0(B,&U,NULL,NULL) == FAIL)
	    goto err;
	  if ( flag ) 
	    for ( i = 0 ; i < B->m ; i++)
	      {
		if ( B->R[i+B->m*i]== 0.0 ) 
		  {
		    Sciprintf("Warning: computing %s of a singular matrix \n",name);
		    break;
		  }
	      }
	  for ( i = 0 ; i < B->m ; i++) vp->R[i]= Fr(B->R[i+B->m*i]);
	  /* now compute U*diag(log(vp))*U' */
	  for ( i = 0 ; i < B->m ; i++)
	    for ( j = 0 ; j < B->n ; j++)
	      B->R[i+j*U->m]= vp->R[j]*U->R[i+j*U->m];
	  if ( (Res = nsp_mat_mult(B,U,2)) == NULL) goto err;
	}
      else
	{
	  if ( nsp_zgees0(B,&U,NULL,NULL) == FAIL)
	    goto err;
	  if ( flag ) 
	    for ( i = 0 ; i < B->m ; i++)
	      {
		if ( B->C[i+B->m*i].r == 0.0 && B->C[i+B->m*i].i == 0.0 ) 
		  {
		    Sciprintf("Warning: computing %s of a singular matrix \n",name);
		    break;
		  }
	      }
	  for ( i = 0 ; i < B->m ; i++) Fc(&B->C[i+B->m*i],&vp->C[i]);
	  /* now compute U*diag(log(vp))*U' */
	  for ( i = 0 ; i < B->m ; i++)
	    for ( j = 0 ; j < B->n ; j++)
	      {
		B->C[i+j*U->m].r = vp->C[j].r * U->C[i+j*U->m].r 
		  - vp->C[j].i *U->C[i+j*U->m].i ;
		B->C[i+j*U->m].i = vp->C[j].r *U->C[i+j*U->m].i 
		  +  vp->C[j].i * U->C[i+j*U->m].r;
	      }
	  if ( (Res = nsp_mat_mult(B,U,2)) == NULL) goto err;
	}
    }
  else
    {
      double  rcond, tol_rcond = Max(A->m,A->n)*nsp_dlamch("eps");
      if ( nsp_mat_complexify(B, 0.0) == FAIL ) goto err;
      if ( nsp_spec(B,&vp,&U) == FAIL) 
	goto err;
      if ( vp->rc_type == 'r' )
	{
	  int complex= FALSE;
	  for ( i = 0 ; i < B->m ; i++) 
	    if ( vp->R[i] < 0 ) 
	      {
		complex= TRUE; 
		break;
	      }
	  if ( complex == TRUE ) 
	    {
	      if ( nsp_mat_complexify(vp, 0.0) == FAIL ) goto err;
	      if ( nsp_mat_complexify(U, 0.0) == FAIL ) goto err;
	    }
	  if ( U->rc_type == 'c' )
	    {
	      /* This should be changed in  nsp_spec, if vp is 
	       * real then U should be real.
	       */
	      if ( nsp_mat_complexify(vp, 0.0) == FAIL ) goto err;
	    }
	}
      if ( vp->rc_type == 'r' )
	{
	  if ( flag ) 
	    for ( i = 0 ; i < B->m ; i++)
	      {
		if ( vp->R[i] == 0.0 )
		  {
		    Sciprintf("Warning: computing %s of a singular matrix \n",name);
		    break;
		  }
	      }
	  for ( i = 0 ; i < B->m ; i++) vp->R[i]= Fr(vp->R[i]);
	  /* now compute U*diag(log(vp))*U' */
	  for ( i = 0 ; i < B->m ; i++)
	    for ( j = 0 ; j < B->n ; j++)
	      B->R[i+j*U->m] = vp->R[j]* U->R[i+j*U->m];
	  if ((Res = nsp_matrix_bdiv(B,U, tol_rcond)) == NULL) goto err;
	  if ( nsp_inv(U, &rcond, FALSE) == FAIL || rcond <= tol_rcond )
	      goto err;
	  if ( (Res = nsp_mat_mult(B,U,0)) == NULL) goto err;
	}
      else
	{
	  if ( flag )
	    for ( i = 0 ; i < B->m ; i++)
	      {
		if ( vp->C[i].r == 0.0 && vp->C[i].i == 0.0 ) 
		  {
		    Sciprintf("Warning: computing %s of a singular matrix \n",name);
		    break;
		  }
	      }
	  for ( i = 0 ; i < B->m ; i++) Fc(&vp->C[i],&vp->C[i]);
	  /* now compute U*diag(log(vp))*U' */
	  for ( i = 0 ; i < B->m ; i++)
	    for ( j = 0 ; j < B->n ; j++)
	      {
		B->C[i+j*U->m].r = vp->C[j].r * U->C[i+j*U->m].r 
		  - vp->C[j].i *U->C[i+j*U->m].i ;
		B->C[i+j*U->m].i = vp->C[j].r *U->C[i+j*U->m].i 
		  +  vp->C[j].i * U->C[i+j*U->m].r;
	      }
	  if ( nsp_inv(U, &rcond, FALSE) == FAIL || rcond <= tol_rcond ) goto err;
	  if ( (Res = nsp_mat_mult(B,U,0)) == NULL) goto err;
	}
    }
  nsp_matrix_destroy(B);
  nsp_matrix_destroy(U);
  nsp_matrix_destroy(vp);
  return Res;

 err:
  Scierror("Error: %s fails\n",name); 
  nsp_matrix_destroy(B);
  nsp_matrix_destroy(U);
  nsp_matrix_destroy(vp);
  nsp_matrix_destroy(Res);
  return NULLMAT;
}


NspMatrix *nsp_matrix_logm(NspMatrix *A, funFr Fr, funFc Fc,char *name, int flag)
{
  return nsp_matrix_funm(A,log,nsp_log_c,"logm",TRUE);
}

NspMatrix *nsp_matrix_sqrtm(NspMatrix *A, funFr Fr, funFc Fc,char *name, int flag)
{
  return nsp_matrix_funm(A,sqrt,nsp_sqrt_c,"sqrtm",TRUE);
}


