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

/*
 * xerbla_:
 * switch lapack message to nsp message
 * Return value: 
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
		      NspMatrix **rank,double *tol,char flag);
static int intzgeqrpf(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E,
		      NspMatrix **rank,double *tol,char flag);

/* OK 
 * QR decomposition  
 * mode can be 'x' or 'e' (economic) 
 * rank and E are computed if non null arguments are transmited 
 * tol can be given or computed if null is transmited 
 */

int nsp_qr(NspMatrix *A,NspMatrix **q,NspMatrix **r,NspMatrix **rank,NspMatrix **E,double *tol,char mode)
{
  if (A->rc_type == 'r' ) 
    return  intdgeqrpf(A,q,r,E,rank,tol,mode);
  else 
    return  intzgeqrpf(A,q,r,E,rank,tol,mode);
}

/*  with real NspMatrix A */ 

static int intdgeqrpf(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E,
		      NspMatrix **rank,double *tol,char flag)
{
  NspMatrix *jpvt,*tau,*work;
  int  workMin, info, *Ijpvt, m = A->m,n=A->n,Minmn= Min(m,n);

  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  {
    if (( *Q =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if (( *R =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if ( E != NULL)
      {
	if (( *E =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }
  /* Q is an mxm matrix or mxMinmn*/ 
  if (( *Q =nsp_matrix_create(NVOID,A->rc_type,m,(flag == 'e') ? Minmn : m)) == NULLMAT) return FAIL;
  /* R is a copy of A: */
  if (( *R =nsp_matrix_copy(A)) == NULLMAT) return FAIL ; 

  if ( E != NULL ) {
    if ((*E =nsp_matrix_create(NVOID,A->rc_type,n,n))== NULLMAT) return FAIL;
    /* jpvt is int NspMatrix */ 
    if ((jpvt =nsp_matrix_create(NVOID,'r',1,n))== NULLMAT) return FAIL;
  }
  if ((tau =nsp_matrix_create(NVOID,'r',1,Min(m,n)))== NULLMAT) return FAIL;
  
  /* Max(m,n) for dorgqr */ 
  workMin =( E == NULL) ? ( Max(1,Max(m,n))) :  Max(n,m) * 3;
  
  if ((work =nsp_matrix_create(NVOID,'r',1,workMin)) == NULLMAT) return FAIL;

  if ( E == NULL ) 
    {
      C2F(dgeqrf)(&(m),&(n),(*R)->R,&(m),tau->R,work->R,&workMin,
		  &info);
    } 
  else 
    {
      int ix;
      Ijpvt = (int *) jpvt->R;
      for (ix = 0; ix < jpvt->mn ; ++ix) Ijpvt[ix]= 0; 
      C2F(dgeqpf)(&(m),&(n),(*R)->R,&(m),Ijpvt,tau->R,work->R, &info);
      /*     SUBROUTINE DGEQPF( M, N, A, LDA, JPVT, TAU, WORK, INFO ) */
    }
  if (info != 0) {
    Scierror("Error: something wrong in dgeqpf\n"); 
    return FAIL;
  }

  /* Now we must extract Q from R and tau  */ 

  if (m > n) 
    {
      int j,ix,ij;
      /* Q = [ R,0 ] */ 
      C2F(dlacpy)("F", &m, &n,(*R)->R, &m,(*Q)->R, &m, 1L);
      if ( flag != 'e' ) {
	for (j = n + 1; j <= m ; ++j) {
	  for (ix = 1; ix <= m ; ++ix) {
	    ij = ix + (j - 1) * m;
	    (*Q)->R[ ij - 1] = 0.;
	  }
	}
      }
    } else {
      /* Q = R(1:m,1:m) */ 
      C2F(dlacpy)("F", &m, &m, (*R)->R, &m, (*Q)->R, &m, 1L);
    }

  if ( flag == 'e') 
    C2F(dorgqr)(&m, &Minmn,&Minmn, (*Q)->R, &m, tau->R, work->R, &workMin, &info);
  else 
    C2F(dorgqr)(&m, &m,&Minmn, (*Q)->R, &m, tau->R, work->R, &workMin, &info);
  if (info != 0) {
    Scierror("Error: something wrong dorgqr\n"); 
    return FAIL;
  }
  /* Extract R from R  */ 
  nsp_mat_triu(*R,0); 
  /* compute E if requested */ 
  if ( E != NULL ) 
    {
      double zero = 0.0;
      int j;
      C2F(dlaset)("F", &n, &n, &zero, &zero,(*E)->R, &n, 1L);
      for (j = 0; j < n; ++j)  (*E)->R[Ijpvt[j]-1 + j* n]=1;
    }

  /* if requested we compute the rank : 
   * Note that, since R diagonal values are assumed to be in decreasing order 
   * the rank is corectly computed if E is also computed 
   */ 

  if (rank != NULL ) {
    int k=0,j;
    double *Rval = (*R)->R ; 
    double tt = Abs(Rval[0]);
    double Tol;
    double eps = C2F(dlamch)("eps", 3L);  
    if (tol == NULL ) 
      Tol = ((double) Max(m,n)) * eps * tt;
    else 
      Tol = *tol; 
    for (j = 0 ; j < Min(m,n) ; ++j) {
      if ( Abs(Rval[k]) <= Tol ) break; 
      k += m+1;
    }
    if ((*rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
    (*rank)->R[0]=j; 
  }
  
  if ( flag == 'e' && m-n > 0 )
    {
      int i;
      nsp_matrix_destroy(work); 
      /* we must delete the last rows of R */ 
      if (( work =nsp_matrix_create(NVOID,'r',1,m-n)) == NULLMAT) return FAIL;
      for ( i = n+1; i <= m ; i++) work->R[i-(n+1)]=i;
      if (nsp_matrix_delete_rows(*R,work) == FAIL) return FAIL; 
    }
  /* clean workspace */ 
  nsp_matrix_destroy(tau); 
  nsp_matrix_destroy(work); 
  if ( E != NULL)  { nsp_matrix_destroy(jpvt);}
  return OK;
}


/* with complex NspMatrix A */ 

static int intzgeqrpf(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E,
		      NspMatrix **rank,double *tol,char flag)
{
  NspMatrix *jpvt,*tau,*work,*rwork;
  int  workMin, info, *Ijpvt, m = A->m,n=A->n,Minmn= Min(m,n);
  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  {
    if (( *Q =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if (( *R =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if ( E != NULL)
      {
	if (( *E =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }
  /* Q is an mxm matrix or mxMinmn*/ 
  if (( *Q =nsp_matrix_create(NVOID,A->rc_type,m,(flag == 'e') ? Minmn: m)) == NULLMAT) return FAIL;
  /* R is a copy of A: */
  if (( *R =nsp_matrix_copy(A)) == NULLMAT) return FAIL ; 

  if ( E != NULL ) {
    /* E is a real matrix */
    if ((*E =nsp_matrix_create(NVOID,'r',n,n))== NULLMAT) return FAIL;
    /* jpvt is int NspMatrix */ 
    if ((jpvt =nsp_matrix_create(NVOID,'r',1,n))== NULLMAT) return FAIL;
  }
  if ((tau =nsp_matrix_create(NVOID,A->rc_type,1,Min(m,n)))== NULLMAT) return FAIL;


  if ((rwork =nsp_matrix_create(NVOID,'r',1,2*n)) == NULLMAT) return FAIL;

  /* Max(m,n) for zungqr */ 
  workMin = Max(m,n);
  if ((work =nsp_matrix_create(NVOID,'c',1,workMin)) == NULLMAT) return FAIL;
  
  if ( E == NULL) 
    {
      /* XXX make a first query to compute optimal work space ? */
      C2F(zgeqrf)(&m, &n,(*R)->C, &m, tau->C, work->C, &workMin, &info);
    } 
  else 
    {
      int ix;
      Ijpvt = (int *) jpvt->R;
      for (ix = 0; ix < n ; ++ix) Ijpvt[ix] =0;
      C2F(zgeqpf)(&m, &n,(*R)->C, &m, Ijpvt,tau->C,work->C,rwork->R, &info);
    }
  if (info != 0) {
    Scierror("Error: something wrong in zgeqpf\n"); 
    return FAIL;
  }
  /* Now we must extract Q from R and tau  */ 

  if (m > n) {
    static doubleC zero = {0.0,0.0};
    int j,ix,ij;
    /* Q = [ R,0 ] */ 
    C2F(zlacpy)("F", &m, &n,(*R)->C, &m,(*Q)->C, &m, 1L);
    if ( flag != 'e' ) {
      for (j = n + 1; j <= m ; ++j) {
	for (ix = 1; ix <= m ; ++ix) {
	  ij = ix + (j - 1) * m;
	  (*Q)->C[ ij - 1] = zero;
	}
      }
    }
  } else {
    /* Q = R(1:m,1:m) */ 
    C2F(zlacpy)("F", &m, &m, (*R)->C, &m, (*Q)->C, &m, 1L);
  }
  
  if ( flag == 'e')   
    C2F(zungqr)(&m, &Minmn,&Minmn, (*Q)->C, &m, tau->C, work->C, &workMin, &info);
  else 
    C2F(zungqr)(&m, &m,&Minmn, (*Q)->C, &m, tau->C, work->C, &workMin, &info);
  if (info != 0) {
    Scierror("Error: something wrong in zungqr\n"); 
    return FAIL;
  }

  /* Now we extract R from R  */ 
  nsp_mat_triu(*R,0); 

  /* And if necessary we compute E */ 

  if ( E != NULL ) {
    double zero = 0.0;
    int j,ij;
    C2F(dlaset)("F", &n, &n, &zero, &zero,(*E)->R, &n, 1L);
    for (j = 1; j <= n; ++j) {
      ij = Ijpvt[ j - 1] + (j - 1) * n;
      (*E)->R[ij - 1] = 1.;
    }
  }

  /* if requested we compute the rank : 
   * Note: since R diagonal values are assumed to be in decreasing order 
   * here the rank is corectly computed if E is also computed 
   */ 

  if (rank != NULL ) {
    int k=0,j;
    doubleC *Rval = (*R)->C ; 
    double tt = nsp_abs_c(Rval);
    double Tol;
    double   eps = C2F(dlamch)("eps", 3L);  
    if (tol == NULL ) 
      Tol = ((double) Max(m,n)) * eps * tt;
    else 
      Tol = *tol; 
    for (j = 0 ; j < Min(m,n) ; ++j) {
      if ( nsp_abs_c(&Rval[k]) <= Tol ) break; 
      k += m+1;
    }
    if ((*rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
    (*rank)->R[0]=j; 
  }
  
  
  if ( flag == 'e' && m-n > 0 ) 
    {
      int i;
      nsp_matrix_destroy(work); 
      /* we must delete the last rows of R */ 
      if (( work =nsp_matrix_create(NVOID,'r',1,m-n)) == NULLMAT) return FAIL;
      for ( i = n+1; i <= m ; i++) work->R[i-(n+1)]=i;
      if (nsp_matrix_delete_rows(*R,work) == FAIL) return FAIL; 
    }
  
  /* clean workspace */ 
  nsp_matrix_destroy(tau); 
  nsp_matrix_destroy(work); 
  nsp_matrix_destroy(rwork); 
  if ( E != NULL)  { nsp_matrix_destroy(jpvt);}
  return OK;
}


/* OK 
 * Lu factorization 
 * A is changed by nsp_lu 
 */

int intdgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **U,NspMatrix **E);
int intzgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **U,NspMatrix **E);

int nsp_lu(NspMatrix *A,NspMatrix **L,NspMatrix **U,NspMatrix **E)
{
  if (A->rc_type == 'r' ) 
    return  intdgetrf(A,L,U,E);
  else 
    return  intzgetrf(A,L,U,E);
}

int intdgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **U,NspMatrix **E)
{
  int info, m = A->m, n = A->n ,Minmn,i,j,*Ipiv ;
  NspMatrix *ipiv;

  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  {
    if (( *L =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if ( E != NULL)
      {
	if (( *E =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }

  Minmn = Min(m,n);

  if (( *L =nsp_matrix_create(NVOID,A->rc_type,m,Minmn)) == NULLMAT) return FAIL;
  if (( *U =nsp_matrix_create(NVOID,A->rc_type,Minmn,n)) == NULLMAT) return FAIL;

  /* int matrix XXX*/ 
  if (( ipiv =nsp_matrix_create(NVOID,A->rc_type,1,Minmn)) == NULLMAT) return FAIL;
  Ipiv = (int *) ipiv->R;

  if ( E != NULL) 
    {
      if (( *E =nsp_matrix_create(NVOID,A->rc_type,m,m)) == NULLMAT) return FAIL;
    }

  C2F(dgetrf)(&m, &n,A->R, &m,Ipiv, &info);
  if (info < 0) {
    Scierror("Error: something wrong in lu\n");
    return FAIL;
  }
  
  /* Extract U from A */ 
  for ( i = 0 ; i < (*U)->mn ; i++ ) (*U)->R[i]=0.0; 
  C2F(dlacpy)("U", &Minmn, &n,A->R, &m, (*U)->R, &Minmn, 1L);

  if (E == NULL ) 
    {
      NspMatrix *iinvpiv;
      int *Iinvpiv;
      /* E is not requested L must be changed to E'*L */ 
      /* compute a representation of E' */
      if (( iinvpiv =nsp_matrix_create(NVOID,A->rc_type,1,m)) == NULLMAT) return FAIL;
      Iinvpiv = (int *) iinvpiv->R;
      for ( i=1; i <= m ; i++) Iinvpiv[i-1]=i;
      for ( i=1; i <= Minmn ; i++) 
	{
	  int ip = Ipiv[i-1];
	  if ( ip != i) { 
	    int iw = Iinvpiv[i-1]; 
	    Iinvpiv[i-1] = Iinvpiv[ip-1];
	    Iinvpiv[ip-1] = iw; 
	  }
	}
      /* use A to store L before E' product */ 
      for ( i = 0 ; i < Minmn ; i++ ) 
	{
	  A->R[(m+1)*i]=1.0; 
	  for ( j = i+1; j < Minmn ; j++) A->R[i+j*m]=0.0; 
	}
      /* store E'*L in L */ 
      for (i=0; i < m ; i++ ) 
	{
	  C2F(dcopy)(&Minmn,&A->R[i],&m,&(*L)->R[Iinvpiv[i]-1],&m);
	}
      nsp_matrix_destroy(iinvpiv); 
    }
  else 
    {
      double zero = 0.0,done = 1.0;
      int one = 1;
      /* Extract L from A */ 
      for ( i = 0 ; i < (*L)->mn ; i++ ) (*L)->R[i]=0.0; 
      C2F(dlacpy)("L", &m, &Minmn,A->R, &m,(*L)->R, &m, 1L);
      for ( i = 0 ; i < Minmn ; i++ ) (*L)->R[(m+1)*i]=1.0; 
      /* Compute E */
      C2F(dlaset)("F", &m, &m, &zero, &done,(*E)->R, &m, 1L);
      C2F(dlaswp)(&m,(*E)->R,&m,&one ,&Minmn, Ipiv,&one );
    }
  nsp_matrix_destroy(ipiv);
  return OK;
}


int intzgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **U,NspMatrix **E)
{
  doubleC Czero = {0.0,0.0}; /*  ,Cone ={ 1.0,0.0} ; */
  int info, m = A->m, n = A->n ,Minmn,i,j,*Ipiv ;
  NspMatrix *ipiv;
  
  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  {
    if (( *L =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if ( E != NULL)
      {
	if (( *E =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }

  Minmn = Min(m,n);

  if (( *L =nsp_matrix_create(NVOID,A->rc_type,m,Minmn)) == NULLMAT) return FAIL;
  if (( *U =nsp_matrix_create(NVOID,A->rc_type,Minmn,n)) == NULLMAT) return FAIL;

  /* int matrix XXX*/ 
  if (( ipiv =nsp_matrix_create(NVOID,'r',1,Minmn)) == NULLMAT) return FAIL;
  Ipiv = (int *) ipiv->R;

  if ( E != NULL) 
    {
      if (( *E =nsp_matrix_create(NVOID,'r',m,m)) == NULLMAT) return FAIL;
    }

  C2F(zgetrf)(&m, &n,A->C, &m,Ipiv, &info);
  if (info < 0) {
    Scierror("Error: something wrong in lu\n");
    return FAIL;
  }
  
  /* Extract U from A */ 
  for ( i = 0 ; i < (*U)->mn ; i++ ) (*U)->C[i] = Czero; 
  C2F(zlacpy)("U", &Minmn, &n,A->C, &m, (*U)->C, &Minmn, 1L);

  if (E == NULL ) 
    {
      NspMatrix *iinvpiv;
      int *Iinvpiv;
      /* E is not requested L must be changed to E'*L */ 
      /* compute a representation of E' */
      if (( iinvpiv =nsp_matrix_create(NVOID,'r',1,m)) == NULLMAT) return FAIL;
      Iinvpiv = (int *) iinvpiv->R;
      for ( i=1; i <= m ; i++) Iinvpiv[i-1]=i;
      for ( i=1; i <= Minmn ; i++) 
	{
	  int ip = Ipiv[i-1];
	  if ( ip != i) { 
	    int iw = Iinvpiv[i-1]; 
	    Iinvpiv[i-1] = Iinvpiv[ip-1];
	    Iinvpiv[ip-1] = iw; 
	  }
	}
      /* use A to store L before E' product */ 
      for ( i = 0 ; i < Minmn ; i++ ) 
	{
	  A->C [(m+1)*i].r =1.0; A->C [(m+1)*i].i =0.0; 
	  for ( j = i+1; j < Minmn ; j++) A->C[i+j*m]= Czero; 
	}
      /* store E'*L in L */ 
      for (i=0; i < m ; i++ ) C2F(zcopy)(&Minmn,&A->C[i],&m,&(*L)->C[Iinvpiv[i]-1],&m);
      nsp_matrix_destroy(iinvpiv); 
    }
  else 
    {
      double zero = 0.0,done = 1.0;
      int one = 1;
      /* Extract L from A */ 
      for ( i = 0 ; i < (*L)->mn ; i++ ) (*L)->C[i]=Czero; 
      C2F(zlacpy)("L", &m, &Minmn,A->C, &m,(*L)->C, &m, 1L);
      for ( i = 0 ; i < Minmn ; i++ ) { (*L)->C[(m+1)*i].r=1.0; (*L)->C[(m+1)*i].i=0.0; }
      /* Compute E */
      /* C2F(zlaset)("F", &m, &m, &Czero, &Cone,(*E)->C, &m, 1L);
       *  C2F(zlaswp)(&m,(*E)->C,&m,&one ,&Minmn, Ipiv,&one ); 
       */
      /* Compute E as a real matrix */
      C2F(dlaset)("F", &m, &m, &zero, &done,(*E)->R, &m, 1L);
      C2F(dlaswp)(&m,(*E)->R,&m,&one ,&Minmn, Ipiv,&one );

    }
  nsp_matrix_destroy(ipiv);
  return OK;
}

/* OK 
 * svd: 
 *    A is modified 
 *    S is always computed 
 *    if U != NULL then 
 *       U and V are computed. 
 *       if flag is set to "S" then Min(m,n) columns 
 *       are computed for U and V (rows of Vt) else 
 *      U and V are fully computed 
 *    rank is computed if non null (using tol if tol != NULL)
 */

static int intdgesvd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol);

static int intzgesvd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol);

int nsp_svd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol)
{
  if (( A =nsp_matrix_copy(A) )== NULLMAT) return FAIL;
  if ( A->rc_type == 'r' ) 
    {
      if ( intdgesvd(A,S,U,V,flag,Rank,tol) == FAIL) return FAIL;
    } 
  else
    {
      if ( intzgesvd(A,S,U,V,flag,Rank,tol) == FAIL) return FAIL;
    }
  return OK;
}

static int intdgesvd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,
		     NspMatrix **Rank,double *tol)
{
  int m = A->m,n=A->n,ix1,ix2,lworkMin,info,i;
  int Minmn = Min(m,n);
  NspMatrix *SV,*Vt,*dwork; 

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ( U != NULL)
      {
	if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
	if (( *V =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    if (( *S =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if ( Rank != NULL) 
      {
	if ((*Rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
	(*Rank)->R[0] = 0.0;
      }
    return OK ; 
  }

  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (isinf (A->R[i]) || isnan (A->R[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

  if ((SV =nsp_matrix_create(NVOID,'r',Minmn,1)) == NULLMAT) return FAIL;
  if ( U != NULL ) {
    /* we also compute U and Vt */
    int nU  = ( flag == 'S') ? Minmn : m;
    int mVt = ( flag == 'S') ? Minmn : n;
    if ((*U =nsp_matrix_create(NVOID,'r',m,nU)) == NULLMAT) return FAIL;
    if ((Vt =nsp_matrix_create(NVOID,'r',mVt,n)) == NULLMAT) return FAIL;
  }
  
  /* computing work space area */ 

  ix1 = Minmn * 3 + Max(m,n), ix2 = Minmn * 5;
  lworkMin = Max(ix1,ix2);
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;

  if ( U == NULL) 
    {
      /* just compute the singular values */ 
      C2F(dgesvd)("N","N",&m, &n,A->R,&m,SV->R,NULL,&m,NULL, &n,
		  dwork->R, &lworkMin, &info, 1L, 1L);
      if (info != 0) {
	if (info > 0) {
	  Scierror("Error: convergence problem in svd\n"); 
	} 
	/* message for info < 0 is given by xerbla.c */
	return FAIL;
      }
      /*  next lines to patch an error of DGESVD */
      nsp_mat_abs(SV); 
      /*  sort sv */ 
      C2F(dlasrt)("D", &Minmn,SV->R, &info, 1L);
    } 
  else 
    {
      if ( flag== 'S' ) 
	{
	  /* compute the singular values and Min(m,n) cols of u and rows of vt */ 
	  C2F(dgesvd)("S","S", &m, &n,A->R, &m,SV->R,(*U)->R,&m,Vt->R,&Vt->m,
		      dwork->R, &lworkMin, &info, 1L, 1L);
	}
      else 
	{
	  /* compute the singular values u and vt */ 
	  C2F(dgesvd)("A","A", &m, &n,A->R, &m,SV->R,(*U)->R, &m,Vt->R, &n,
		      dwork->R, &lworkMin, &info, 1L, 1L);
	}
    }

  if (info != 0) {
    if (info > 0) {
      Scierror("Error: convergence problem in svd\n"); 
    } 
    /* message for info < 0 is given by xerbla.c */
    return FAIL;
  }

  if ( U != NULL )
    {
      /* Build a matrix from the singular values array */ 
      if ( flag == 'S' ) 
	{
	  if ((*S =nsp_mat_zeros(Minmn,Minmn)) == NULLMAT) return FAIL;
	  nsp_matrix_set_diag(*S,SV,0);
	}
      else
	{
	  if ((*S =nsp_mat_zeros(m,n)) == NULLMAT) return FAIL;
	  nsp_matrix_set_diag(*S,SV,0);
	}
    }
  else
    {
      *S = SV;
    }
  
  /* build V from its transpose matrix Vt */ 
  
  if ( U != NULL && (( *V =nsp_matrix_transpose(Vt)) == NULLMAT)) return FAIL;

  /* compute the rank if requested */ 

  if ( Rank != NULL) 
    {
      int i;
      double eps = C2F(dlamch)("eps", 3L);
      double Tol = ( tol == NULL) ? Max(m,n) * eps * SV->R[0] : *tol ; 
      int irank =0 ; 
      for (i = 0 ; i < Minmn; ++i) {
	if ( SV->R[i] > Tol) irank = i+1;
	else break;
      }
      if ((*Rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
      (*Rank)->R[0] = (double) irank ; 
    }
  nsp_matrix_destroy(dwork) ; 

  if ( U != NULL)
    {
      nsp_matrix_destroy(Vt) ; 
      nsp_matrix_destroy(SV) ; 
    }
  return OK;
} 


static int intzgesvd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol)
{
  int m = A->m,n=A->n,lworkMin,info,i;
  int Minmn = Min(m,n), lrwork;
  NspMatrix *SV,*Vt,*dwork,*rwork; 

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ( U != NULL)
      {
	if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
	if (( *V =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    if (( *S =nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT) return FAIL;
    if ( Rank != NULL) 
      {
	if ((*Rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
	(*Rank)->R[0] = 0.0;
      }
    return OK ; 
  }

  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (nsp_isinf_c (&A->C[i]) || nsp_isnan_c (&A->C[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

  if ((SV =nsp_matrix_create(NVOID,'r',Minmn,1)) == NULLMAT) return FAIL;
  if ( U != NULL ) {
    /* we also compute U and Vt */
    int nU  = ( flag == 'S') ? Minmn : m;
    int mVt = ( flag == 'S') ? Minmn : n;
    if ((*U =nsp_matrix_create(NVOID,'c',m,nU)) == NULLMAT) return FAIL;
    if ((Vt =nsp_matrix_create(NVOID,'c',mVt,n)) == NULLMAT) return FAIL;
  }
  
  /* computing work space area */ 

  lrwork = Max(Minmn * 3,Minmn * 5) ;
  if (( rwork =nsp_matrix_create(NVOID,'r',1,lrwork)) == NULLMAT) return FAIL;

  lworkMin = (Minmn*2) + Max(m,n);
  if (( dwork =nsp_matrix_create(NVOID,'c',1,lworkMin)) == NULLMAT) return FAIL;

  if ( U == NULL) 
    {
      /* just compute the singular values */ 
      C2F(zgesvd)("N","N",&m, &n,A->C,&m,SV->R,NULL,&m,NULL, &n,
		  dwork->C, &lworkMin,rwork->R, &info, 1L, 1L);
      /*  next lines to patch an error of DGESVD */
      if (info != 0) {
	if (info > 0) {
	  Scierror("Error: convergence problem in svd\n"); 
	} 
	/* message for info < 0 is given by xerbla.c */
	return FAIL;
      }
      nsp_mat_abs(SV); 
      /*  sort sv */ 
      C2F(dlasrt)("D", &Minmn,SV->R, &info, 1L);
    } 
  else 
    {
      if ( flag == 'S' ) 
	{
	  /* compute the singular values and Min(m,n) cols of u and rows of vt */ 
	  C2F(zgesvd)("S","S", &m, &n,A->C, &m,SV->R,(*U)->C,&m,Vt->C,&Vt->m,
		      dwork->C, &lworkMin,rwork->R, &info, 1L, 1L);
	}
      else 
	{
	  /* compute the singular values u and vt */ 
	  C2F(zgesvd)("A","A", &m, &n,A->C, &m,SV->R,(*U)->C, &m,Vt->C, &n,
		      dwork->C, &lworkMin,rwork->R, &info, 1L, 1L);
	}
    }

  if (info != 0) {
    if (info > 0) {
      Scierror("Error: convergence problem in svd\n"); 
    } 
    return FAIL;
  }

  /* Build a matrix from the singular values array */ 

  if ( U != NULL )
    {
      /* Build a matrix from the singular values array */ 
      if ( flag == 'S' ) 
	{
	  if ((*S =nsp_mat_zeros(Minmn,Minmn)) == NULLMAT) return FAIL;
	  nsp_matrix_set_diag(*S,SV,0);
	}
      else
	{
	  if ((*S =nsp_mat_zeros(m,n)) == NULLMAT) return FAIL;
	  nsp_matrix_set_diag(*S,SV,0);
	}
    }
  else
    {
      *S = SV;
    }
  
  /* build V from its transpose matrix Vt */
  /* note that tranposition on complex NspMatrix is the conjugate transpose */ 
  
  if ( U != NULL && (( *V =nsp_matrix_transpose(Vt)) == NULLMAT)) return FAIL;

  /* compute the rank if requested */ 

  if ( Rank != NULL) {
    int i;
    double eps = C2F(dlamch)("eps", 3L);
    double Tol = ( tol == NULL) ? Max(m,n) * eps * SV->R[0] : *tol ; 
    int irank =0 ; 
    for (i = 0 ; i < Minmn; ++i) {
      if ( SV->R[i] > Tol) irank = i+1;
      else break;
    }
    if ((*Rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
    (*Rank)->R[0] = (double) irank ; 
  }

  nsp_matrix_destroy(dwork) ; 
  nsp_matrix_destroy(rwork) ; 
  if ( U!= NULL) 
    {
      nsp_matrix_destroy(Vt) ; 
      nsp_matrix_destroy(SV) ; 
    }
  return OK;

}

/* OK
 * nsp_spec 
 * spec(A) for nonsymmetric matrices dgeev 
 * A is changed, if v != NULL v is computed 
 */

static int intdgeev(NspMatrix *A,NspMatrix **d,NspMatrix **v); 
static int intzgeev(NspMatrix *A,NspMatrix **d,NspMatrix **v);

int nsp_spec(NspMatrix *A, NspMatrix **d,NspMatrix **v) 
{
  if ( A->rc_type == 'r' ) 
    return  intdgeev(A,d,v); 
  else 
    return  intzgeev(A,d,v); 
}

static int intdgeev(NspMatrix *A,NspMatrix **d,NspMatrix **v) 
{
  char type = 'r';
  int m = A->m,n=A->n;
  int info, lworkMin,i,j;
  NspMatrix *dwork,*wr,*wi,*vr;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if (( *d =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if ( v != NULL) 
      {
	if (( *v =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument for spec should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (isinf (A->R[i]) || isnan (A->R[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

  if (( wr =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;
  if (( wi =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;

  lworkMin = ( v == NULL) ?  n*3 : n*4 ;
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;
  
  if (v == NULL ) 
    {
      C2F(dgeev)("N","N", &n,A->R, &n,wr->R,wi->R,NULL,&n,NULL,&n,
		 dwork->R,&lworkMin, &info, 1L, 1L);
      
    } 
  else 
    {
      if (( vr =nsp_matrix_create(NVOID,'r',n,n)) == NULLMAT) return FAIL;
      C2F(dgeev)("N","V", &n,A->R, &n,wr->R,wi->R,NULL,&n,vr->R,&n,
		 dwork->R,&lworkMin, &info, 1L, 1L);
    }
  
  if (info != 0) {
    Scierror("Error: convergence problem in dgeev\n"); 
    return FAIL;
  }

  /* result is real ? or complex */ 

  for (i = 0 ; i < n ; ++i) { if (wi->R[i] != 0.0) { type = 'c'; break;}}

  /* extract results */ 

  if ( v == NULL ) 
    {
      /* the spec is copied into d */ 
      if (( *d =nsp_matrix_create(NVOID,type,n,1)) == NULLMAT) return FAIL;
      if ( type == 'r' )
	for (i = 0 ; i < n ; ++i) { (*d)->R[i] = wr->R[i]; }
      else 
	for (i = 0 ; i < n ; ++i) { (*d)->C[i].r = wr->R[i]; (*d)->C[i].i = wi->R[i]; }
    }
  else 
    {
      /* diagonal of d is set with spec (wr,wi) 
       * and right engeinvectors in v 
       */ 

      static doubleC  zero= { 0.0,0.0};
      static double   dzero=0.0;
      if (( *v =nsp_matrix_create(NVOID,type,n,n)) == NULLMAT) return FAIL;
      if (( *d =nsp_matrix_create(NVOID,type,n,n)) == NULLMAT) return FAIL;

      if ( type == 'r' ) 
	{
	  C2F(dlaset)("F", &n, &n, &dzero, &dzero,(*d)->R, &n, 1L);
	  for ( i = 0;  i < n ; ++i) { (*d)->R[i*(m+1)] =  wr->R[i]; }
	  for ( i = 0;  i < n*n ; ++i) (*v)->R[i] = vr->R[i]; 
	} 
      else 
	{
	  C2F(zlaset)("F", &n, &n, &zero, &zero,(*d)->C, &n, 1L);
	  for ( i = 0;  i < n ; ++i) 
	    { (*d)->C[i*(m+1)].r =  wr->R[i]; (*d)->C[i*(m+1)].i = wi->R[i]; }
	  /* store right eigenvectors in v */ 
	  /* we could check if XXXX v is real or imag before */
	  j = -1;
	  while (1) 
	    {
	      j ++ ; 
	      if (j >= n) break; 
	      if( wi->R[j] == 0.0 ) 
		for ( i = 0;  i < n ; ++i) 
		  { 
		    int k = i+j*n ;
		    (*v)->C[k].r = vr->R[k]; 
		    (*v)->C[k].i = 0.0;
		  }
	      else if ( wi->R[j] > 0.0 ) 
		{
		  /* complex conjugate case */
		  for ( i = 0;  i < n ; ++i) 
		    { 
		      int k = i+j*n ;
		      (*v)->C[k].r = vr->R[k]; 
		      (*v)->C[k].i = vr->R[k+n]; 
		      (*v)->C[k+n].r = vr->R[k]; 
		      (*v)->C[k+n].i = - vr->R[k+n]; 
		    }
		  j++; 
		}
	    }
	}
    }

  nsp_matrix_destroy(dwork); 
  nsp_matrix_destroy(wr); 
  nsp_matrix_destroy(wi); 
  if (v != NULL )nsp_matrix_destroy(vr); 
  return OK;
}


static int intzgeev(NspMatrix *A,NspMatrix **d,NspMatrix **v) 
{
  char type = 'r';
  int m = A->m,n=A->n;
  int info, lworkMin,i;
  NspMatrix *dwork,*rwork;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if (( *d =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if ( v != NULL) 
      {
	if (( *v =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument for spec should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (nsp_isinf_c (&A->C[i]) || nsp_isnan_c (&A->C[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }


  if (( *d =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT) return FAIL;

  if (( rwork =nsp_matrix_create(NVOID,'r',2*n,1)) == NULLMAT) return FAIL;
  lworkMin = 2*n;
  if (( dwork =nsp_matrix_create(NVOID,'c',1,lworkMin)) == NULLMAT) return FAIL;
  
  if (v == NULL ) 
    {
      C2F(zgeev)("N","N", &n,A->C, &n,(*d)->C,NULL,&n,NULL,&n,dwork->C,&lworkMin,rwork->R,
		 &info, 1L, 1L);
    } 
  else 
    {
      if (( *v =nsp_matrix_create(NVOID,'c',n,n)) == NULLMAT) return FAIL;
      C2F(zgeev)("N","V", &n,A->C, &n,(*d)->C,NULL,&n,(*v)->C,&n,dwork->C,&lworkMin,rwork->R,
		 &info, 1L, 1L);
    }
  
  if (info != 0) {
    Scierror("Error: convergence problem in zgeev\n"); 
    return FAIL;
  }

  /* result is real ? or complex */ 

  for (i = 0 ; i < n ; ++i) { if ((*d)->C[i].i != 0.0) { type = 'c'; break;}}

  /* extract results */ 

  if ( v == NULL ) 
    {
      /* the spec is copied into d */ 
      if ( type == 'r' ) {
	if (nsp_mat_get_real(*d) == FAIL ) return FAIL;
      }
    }
  else 
    {
      /* diagonal of d is set with spec (wr,wi) 
       * and right engeinvectors in v 
       */ 
      NspMatrix *md;
      static doubleC  zero= { 0.0,0.0};
      static double   dzero=0.0;
      if (( md =nsp_matrix_create(NVOID,type,n,n)) == NULLMAT) return FAIL;
      
      if ( type == 'r' ) 
	{
	  C2F(dlaset)("F", &n, &n, &dzero, &dzero,md->R, &n, 1L);
	  for ( i = 0;  i < n ; ++i) { md->R[i*(m+1)] =  (*d)->C[i].r; }
	} 
      else 
	{
	  C2F(zlaset)("F", &n, &n, &zero, &zero,md->C, &n, 1L);
	  for ( i = 0;  i < n ; ++i)  md->C[i*(m+1)] =  (*d)->C[i]; 
	}
      nsp_matrix_destroy(*d); 
      *d = md;
    }
      
  nsp_matrix_destroy(dwork); 
  nsp_matrix_destroy(rwork); 
  return OK;
}


/* OK
 * nsp_spec_sym 
 * spec(A) for symmetric matrices dgeev 
 * if flag == 'V',  V is returned in A 
 */

static int intdsyev(NspMatrix *A,NspMatrix **d,char flag);
static int intzheev(NspMatrix *A,NspMatrix **d,char flag);

int nsp_spec_sym(NspMatrix *A,NspMatrix **d,char flag)
{
  if ( A->rc_type == 'r' ) 
    return  intdsyev(A,d,flag); 
  else 
    return  intzheev(A,d,flag); 
}

static int intdsyev(NspMatrix *A,NspMatrix **d,char flag)
{
  int m = A->m,n=A->n;
  int info, lworkMin,i;
  NspMatrix *dwork,*wr;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if (( *d =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of spec should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (isinf (A->R[i]) || isnan (A->R[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

  if (( wr =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;

  lworkMin =  n*3;
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;
  
  C2F(dsyev)((flag == 'V' ) ? "V" : "N","U" , &n,A->R, &n,wr->R,dwork->R,&lworkMin, &info, 1L, 1L);
  
  if (info != 0) {
    Scierror("Error: convergence problem in dsyev\n"); 
    return FAIL;
  }

  /* extract results */ 

  if ( flag != 'V' ) 
    {
      *d = wr ; 
    }
  else 
    {
      /* diagonal of d is set with spec (wr,wi) 
       * and right engeinvectors in v 
       */ 
      if (( *d =nsp_mat_zeros(m,n)) == NULLMAT) return FAIL;
      for ( i = 0;  i < n ; ++i) { (*d)->R[i*(m+1)] =  wr->R[i]; }
      nsp_matrix_destroy(wr); 
    }

  nsp_matrix_destroy(dwork); 
  return OK;
}

static int intzheev(NspMatrix *A,NspMatrix **d,char flag)
{
  int m = A->m,n=A->n;
  int info, lworkMin,i;
  NspMatrix *dwork,*wr,*rwork;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if (( *d =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of spec should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (nsp_isinf_c (&A->C[i]) || nsp_isnan_c (&A->C[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }


  if (( wr =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;

  if (( rwork =nsp_matrix_create(NVOID,'r',1,n*3-2)) == NULLMAT) return FAIL;
  lworkMin =  n*2;
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;
  
  C2F(zheev)((flag == 'V' ) ? "V" : "N","U" , &n,A->C, &n,wr->R,dwork->C,&lworkMin,rwork->R, &info, 1L, 1L);
  
  if (info != 0) {
    Scierror("Error: convergence problem in zheev\n"); 
    return FAIL;
  }

  /* extract results */ 

  if ( flag != 'V' ) 
    {
      *d = wr ; 
    }
  else 
    {
      /* diagonal of d is set with spec (wr,wi) 
       * and right engeinvectors in v 
       */ 
      if (( *d =nsp_mat_zeros(m,n)) == NULLMAT) return FAIL;
      for ( i = 0;  i < n ; ++i) { (*d)->R[i*(m+1)] =  wr->R[i]; }
      nsp_matrix_destroy(wr); 
    }

  nsp_matrix_destroy(dwork); 
  nsp_matrix_destroy(rwork); 
  return OK;
}

/* OK 
 * nsp_inv 
 */

static int intdgetri(NspMatrix *A);
static int intzgetri(NspMatrix *A);

int nsp_inv(NspMatrix *A) 
{
  if ( A->rc_type == 'r') 
    return intdgetri(A);
  else 
    return intzgetri(A);
}


static int intdgetri(NspMatrix *A)
{
  NspMatrix *iwork,*dwork;
  int *Iwork;
  int info,lworkMin, m = A->m, n = A->n ;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 )  return OK ; 
  
  if (m != n) { 
    Scierror("Error: first argument of inv should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  /* int NspMatrix XXX */ 
  if (( iwork =nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return FAIL;
  Iwork = (int *) iwork->R;

  lworkMin = Max(1,n);
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;
  
  C2F(dgetrf)(&n, &n,A->R, &n, Iwork, &info);
  if (info > 0) {
    Scierror("inv: problem is singular\n");
    return FAIL;
  } else if (info < 0) {
    return FAIL;
  }
  C2F(dgetri)(&n, A->R, &n, Iwork, dwork->R,&lworkMin, &info);
  return OK;
} 

int intzgetri(NspMatrix *A)
{
  NspMatrix *iwork,*dwork;
  int *Iwork;

  int info,lworkMin, m = A->m, n = A->n ;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 )  return OK ; 
  
  if (m != n) { 
    Scierror("Error: first argument of inv should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  /* int NspMatrix XXX */ 
  
  if (( iwork =nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return FAIL;
  Iwork = (int *) iwork->R;

  lworkMin = Max(1,n);
  if (( dwork =nsp_matrix_create(NVOID,'c',1,lworkMin)) == NULLMAT) return FAIL;
  
  C2F(zgetrf)(&n, &n,A->C, &n, Iwork, &info);
  if (info > 0) {
    Scierror("Error: problem is singular\n");
    return FAIL;
  } else if (info < 0) {
    return FAIL;
  }
  C2F(zgetri)(&n, A->C, &n, Iwork, dwork->C,&lworkMin, &info);
  if (info != 0) {
    Scierror("Error: something wrong in zgetri\n");
    return FAIL;
  }
  return OK;

}

/* OK
 * nsp_rcond(A)
 * A is modified 
 */

static int intdgecon(NspMatrix *A,double *rcond);
static int intzgecon(NspMatrix *A,double *rcond);

int nsp_rcond(NspMatrix *A,double *rcond)
{
  if ( A->rc_type == 'r') 
    return intdgecon(A,rcond);
  else 
    return intzgecon(A,rcond);
}


static int intdgecon(NspMatrix *A,double *rcond) 
{
  NspMatrix *iwork,*dwork;
  double anorm;
  int *Iwork, info,lworkMin, m = A->m, n = A->n ;
  *rcond = 0.0;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 )  return OK ; 
  
  if (m != n) { 
    Scierror("Error: first argument of rcond should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  /* int matrix XXX */ 

  if (( iwork =nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return FAIL;
  Iwork = (int *) iwork->R  ;

  lworkMin = 4*n ;
  if (( dwork =nsp_matrix_create(NVOID,A->rc_type,1,lworkMin)) == NULLMAT) return FAIL;

  anorm = C2F(dlange)("1", &n, &n,A->R, &n, NULL, 1L);
  C2F(dgetrf)(&n, &n, A->R, &n, Iwork , &info);
  if (info == 0) {
    C2F(dgecon)("1",&n,A->R,&n,&anorm,rcond,dwork->R,Iwork, &info, 1L);
    if ( info != 0 ) 
      {
	Scierror("Error: something wrong in dgecon\n");
	return FAIL;
      }
      
  } else {
    Scierror("Error: something wrong in dgetrf\n");
    return FAIL;
  }

  nsp_matrix_destroy(dwork); 
  nsp_matrix_destroy(iwork); 
  
  return OK;

} 


static int intzgecon(NspMatrix *A,double *rcond) 
{
  double anorm;
  NspMatrix *iwork,*dwork,*rwork;
  int *Iwork;
  int info,lworkMin, m = A->m, n = A->n ;
  *rcond=0.0;

  /*  A = [] return empty matrices */ 
  if ( A->mn == 0 )  return OK ; 
  
  if (m != n) { 
    Scierror("Error: first argument of rcond should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  /* int matrix XXX */ 

  if (( iwork =nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return FAIL;
  Iwork = (int *) iwork->R  ;

  if (( rwork =nsp_matrix_create(NVOID,'r',1,2*n)) == NULLMAT) return FAIL;
  lworkMin = 2*n ;
  if (( dwork =nsp_matrix_create(NVOID,A->rc_type,1,lworkMin)) == NULLMAT) return FAIL;

  anorm = C2F(zlange)("1", &n, &n,A->C, &n,NULL, 1L);
  C2F(zgetrf)(&n, &n, A->C, &n, Iwork , &info);
  if (info == 0) {
    C2F(zgecon)("1",&n,A->C,&n,&anorm,rcond,dwork->C,rwork->R, &info, 1L);
  } else {
    Scierror("Error: something wrong in zgetrf\n");
    return FAIL;
  }

  nsp_matrix_destroy(dwork); 
  nsp_matrix_destroy(rwork); 
  nsp_matrix_destroy(iwork); 
  
  return OK;
} 

/* OK 
 * nsp_cholewsky
 * A is changed 
 */

int nsp_cholewsky(NspMatrix *A) 
{
  int info, m = A->m, n = A->n ;

  /*  A = [] return empty matrices */ 
  if ( A->mn == 0 )  return OK ; 
  
  if (m != n) { 
    Scierror("Error: first argument of chol should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }
  if ( A->rc_type == 'r' ) 
    C2F(dpotrf)("U", &n,A->R, &n, &info, 1L);
  else 
    C2F(zpotrf)("U", &n,A->C, &n, &info, 1L);
  if (info != 0) {
    if (info > 0) {
      Scierror("Error: matrix is not positive definite\n"); 
    }
    return FAIL;
  }
  nsp_mat_triu(A,0);  
  return OK;
} 

/* OK 
 * det = det(A) 
 * returns d=det or [e,m] with det = m*10^e
 * according to mode 
 * A is changed 
 */

static NspMatrix * intzdet(NspMatrix *A,char mode);
static NspMatrix * intddet(NspMatrix *A,char mode);

NspMatrix * nsp_det(NspMatrix *A,char mode)
{
  if ( A->rc_type == 'r' ) 
    return  intddet(A,mode); 
  else 
    return  intzdet(A,mode); 
}


static NspMatrix * intddet(NspMatrix *A,char mode)
{
  NspMatrix *det;
  int info,ix;
  NspMatrix *ipiv;
  int m = A->m,n=A->n; 
  int *Ipiv;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ((det =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return NULL;
    return det ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument for det should be square and it is (%dx%d)\n", 
	     m,n);
    return NULL;
  }

  /* int matrix XXX */ 
  if (( ipiv =nsp_matrix_create(NVOID,'r',1,m)) == NULLMAT) return NULL;
  Ipiv = (int *) ipiv->R  ;

  C2F(dgetrf)(&m, &n, A->R, &m, Ipiv , &info);
  if (info < 0) {
    Scierror("Error: something wrong in dgetrf\n") ; 
    return NULL;
  }
  
  if (mode == 'd')
    { 
      /* det(A) */
      double dx = 1.;
      /* loop on diag(A) */ 
      for (ix = 0; ix < n ; ++ix) 
	{
	  if ( Ipiv[ix] != ix + 1) dx = -dx; 
	  dx *= A->R[ ix * (m + 1) ];
	}
      if (( det =nsp_matrix_create(NVOID,A->rc_type,1,1)) == NULLMAT) return NULL;
      det->R[0] = dx ;
    } 
  else 
    {    /*  [e,m]=det(A) */
      const double one=1.0, ten= 10.0;
      double dx = 1.0, e = 0.0;
      for (ix = 0; ix < n; ++ix) {
	if ( Ipiv[ix] != ix + 1) dx = -dx; 
	dx *= A->R[ ix * (m + 1) ];
	if (dx == 0.) break ; 
	while ( Abs(dx) < one) {
	  dx *= ten;
	  e -= one; 
	}
	while ( Abs(dx) >= ten ) {
	  dx /= ten; e += one ; 
	}
      }
      if (( det =nsp_matrix_create(NVOID,A->rc_type,1,2)) == NULLMAT) return NULL;
      det->R[0]= e; 
      det->R[1]= dx; 
    } 
  return det ;
}
  

static NspMatrix * intzdet(NspMatrix *A,char mode)
{
  NspMatrix *ipiv,*det;
  int info,ix;
  int m = A->m,n=A->n; 
  int *Ipiv;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if (( det =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return NULL;
    return det;
  }
  
  if (m != n) { 
    Scierror("Error: first argument for det should be square and it is (%dx%d)\n", 
	     m,n);
    return NULL;
  }

  /* int matrix XXX */ 
  if (( ipiv =nsp_matrix_create(NVOID,'r',1,m)) == NULLMAT) return NULL;
  Ipiv = (int *) ipiv->R  ;

  C2F(zgetrf)(&m, &n, A->C, &m, Ipiv , &info);
  if (info < 0) {
    Scierror("Error: something wrong in zgetrf\n") ; 
    return NULL;
  }

  
  if (mode == 'd' ) {    /* det(A) */
    doubleC dx = {1.0,0.0} ;
    /* loop on diag(A) */ 
    for (ix = 0; ix < n ; ++ix) 
      {
	if ( Ipiv[ix] != ix + 1) { dx.r = -dx.r ;  dx.i = -dx.i;};
	nsp_prod_c(&dx,&A->C[ ix * (m + 1) ]);
      }
    if (( det =nsp_matrix_create(NVOID,A->rc_type,1,1)) == NULLMAT) return NULL;
    det->C[0] = dx ;
  } else {    /*  [e,m]=det(A) */
    const double one=1.0, ten= 10.0;
    doubleC dx = {1.0,0.0};
    double e = 0.0;
    for (ix = 0; ix < n; ++ix) {
      if ( Ipiv[ix] != ix + 1) { dx.r = -dx.r ;  dx.i = -dx.i;};
      nsp_prod_c(&dx,&A->C[ ix * (m + 1) ]);
      if ( nsp_abs_c(&dx) == 0.) break ; 
      while ( nsp_abs_c(&dx) < one) {
	dx.r *= ten; dx.i *= ten;
	e -= one; 
      }
      while (nsp_abs_c(&dx) >= ten  ) {
	dx.r /= ten; dx.i /= ten;
	e += one ; 
      }
    }
    if (( det =nsp_matrix_create(NVOID,A->rc_type,1,2)) == NULLMAT) return NULL;
    (det)->C[0].r  = e;    (det)->C[0].i  = 0.0;
    (det)->C[1] = dx; 
  }
  return det;
} 


/* FIXME: unchecked
 * nsp_balanc 
 * [V,D]=balanc(A) 
 * V is stored in A on exit 
 */

static int intdgebal(NspMatrix *A,NspMatrix **D);
static int intzgebal(NspMatrix *A,NspMatrix **D);

int nsp_balanc(NspMatrix *A,NspMatrix **D)
{
  if ( A->rc_type == 'r' ) 
    return  intdgebal(A,D); 
  else 
    return  intzgebal(A,D); 
}

static int intdgebal(NspMatrix *A,NspMatrix **D)
{
  int m = A->m, n = A->n,info,i;
  NspMatrix *work ; 
  int ilo,ihi;

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if (( *D =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of balanc should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }
  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (isinf (A->R[i]) || isnan (A->R[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

  if (( *D =nsp_mat_eye(m,n)) == NULLMAT) return FAIL;
  if (( work =nsp_matrix_create(NVOID,A->rc_type,1,n)) == NULLMAT) return FAIL;

  C2F(dgebal)("B", &n, A->R, &n, &ilo, &ihi, work->R, &info, 1L);
  if (info != 0) {
    Scierror("Error: something wrong in dgebal\n");
    return FAIL;
  }

  C2F(dgebak)("B", "R", &n, &ilo, &ihi, work->R, &n, (*D)->R, &n, &info, 1L, 1L);
  if (info != 0) {
    Scierror("Error: something wrong in dgebak\n");
    return FAIL;
  }
  nsp_matrix_destroy(work);
  return OK;
}

static int intzgebal(NspMatrix *A,NspMatrix **D)
{
  int m = A->m, n = A->n,info,i;
  NspMatrix *work ; 
  int ilo,ihi;
  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if (( *D =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of balanc should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }
  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (nsp_isinf_c (&A->C[i]) || nsp_isnan_c (&A->C[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

  if (( *D =nsp_mat_eye(m,n)) == NULLMAT) return FAIL;
  if (( work =nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return FAIL;

  C2F(zgebal)("B", &n, A->C, &n, &ilo, &ihi, work->R, &info, 1L);
  if (info != 0) {
    Scierror("Error: something wrong in zgebal\n");
    return FAIL;
  }
  C2F(dgebak)("B", "R", &n, &ilo, &ihi, work->R, &n, (*D)->R, &n, &info, 1L, 1L);
    if (info != 0) {
    Scierror("Error: something wrong in zgebak\n");
    return FAIL;
  }

  nsp_matrix_destroy(work);
  return OK;
}


/*
 * nsp_gbalanc
 *     [Ab,Bb,X,Y]=balanc(A,B)
 */

static int intdggbal(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y);
static int intzggbal(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y);

int nsp_gbalanc(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y)
{
  if (( B =nsp_matrix_copy(B) )== NULLMAT) return FAIL;
  if (( A =nsp_matrix_copy(A) )== NULLMAT) return FAIL;
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


static int intdggbal(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y)
{
  NspMatrix *lscale,*rscale,*dwork; 
  int info,lworkMin,ilo,ihi,i;  
  int m = A->m, n = A->n, mb = B->m,nb = B->n ;  
  
  if (m != n) { 
    Scierror("Error: first argument of spec should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  if (mb != nb) { 
    Scierror("Error: second argument of spec should be square and it is (%dx%d)\n", 
	     mb,nb);
    return FAIL;
  }

  if (m != mb || n != nb ) {
    Scierror("Error: spec, first and second arguments must have equal size\n");
    return FAIL;
  }

  /* A = [] return empty matrices */ 

  if ( A->mn == 0 ) {
    if ( X != NULL)
      {
	if (( *X =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    if ( Y != NULL)
      {
	if (( *Y =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }

  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (isinf (A->R[i]) || isnan (A->R[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }
  /* checks that  B != Nan et != Inf */

  for ( i= 0 ; i < B->mn ; i++ ) 
    if (isinf (B->R[i]) || isnan (B->R[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

  if (( lscale =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;
  if (( rscale =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;

  lworkMin = Max(1,6*n);
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;

  C2F(dggbal)("B",&n,A->R, &n,B->R, &n,&ilo,&ihi,lscale->R, rscale->R,dwork->R, &info, 1L);
  if (info != 0) {
    Scierror("Error: something wrong in dggbal\n");
    return FAIL;
  }
  if ( X != NULL) { 
    if (( *X =nsp_mat_eye(m,n)) == NULLMAT) return FAIL;
    C2F(dggbak)("B", "L", &n, &ilo, &ihi, lscale->R, rscale->R, &n,(*X)->R, &n, &info, 1L,1L);
    if (info != 0) {
      Scierror("Error: something wrong in dggbak\n");
      return FAIL;
    }
  }

  if ( Y != NULL ) {
    if (( *Y =nsp_mat_eye(m,n)) == NULLMAT) return FAIL;
    C2F(dggbak)("B", "R", &n, &ilo, &ihi, lscale->R, rscale->R, &n,(*Y)->R, &n, &info, 1L, 1L);
    if (info != 0) {
      Scierror("Error: something wrong in dggbak\n");
      return FAIL;
    }
  }

  /* un peu de ménage XXXXX */ 

  return OK ; 
} 

static int intzggbal(NspMatrix *A,NspMatrix *B,NspMatrix **X,NspMatrix **Y)
{
  NspMatrix *lscale,*rscale,*dwork; 
  int info,lworkMin,ilo,ihi,i;  

  int m = A->m, n = A->n, mb = B->m,nb = B->n ;  
  
  if (m != n) { 
    Scierror("Error: spec, wrong first argument size (%d,%d) should be square \n",m,n);
    return FAIL;
  }

  if (mb != nb) { 
    Scierror("Error: second argument of spec should be square and it is (%dx%d)\n", 
	     mb,nb);
    return FAIL;
  }

  if (m != mb || n != nb ) {
    Scierror("Error: spec, first and second arguments must have equal size\n");
    return FAIL;
  }

  /* A = [] return empty matrices */ 

  if ( A->mn == 0 ) {
    if ( X != NULL)
      {
	if (( *X =nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT) return FAIL;
      }
    if ( Y != NULL)
      {
	if (( *Y =nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }

  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < A->mn ; i++ ) 
    if (nsp_isinf_c (&A->C[i]) || nsp_isnan_c (&A->C[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }
  /* checks that  A != Nan et != Inf */

  for ( i= 0 ; i < B->mn ; i++ ) 
    if (nsp_isinf_c (&B->C[i]) || nsp_isnan_c (&B->C[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

  if (( lscale =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;
  if (( rscale =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;

  lworkMin = Max(1,6*n);
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;

  C2F(zggbal)("B",&n,A->C, &n,B->C, &n,&ilo,&ihi,lscale->R, rscale->R,dwork->R, &info, 1L);
  if (info != 0) {
    Scierror("Error: something wrong in zggbal\n");
    return FAIL;
  }
  if ( X != NULL) { 
    if (( *X =nsp_mat_eye(m,n)) == NULLMAT) return FAIL;
    C2F(dggbak)("B", "L", &n, &ilo, &ihi, lscale->R, rscale->R, &n,(*X)->R, &n, &info, 1L, 1L);
    if (info != 0) {
      Scierror("Error: something wrong in dggbak\n");
      return FAIL;
    }
  }

  if ( Y != NULL ) {
    if (( *Y =nsp_mat_eye(m,n)) == NULLMAT) return FAIL;
    C2F(dggbak)("B", "R", &n, &ilo, &ihi, lscale->R, rscale->R, &n,(*Y)->R, &n, &info, 1L, 1L);
    if (info != 0) {
      Scierror("Error: something wrong in dggbak\n");
      return FAIL;
    }
  }

  /* un peu de ménage XXXXX */ 

  return OK ; 
} 

/* FIXME: unchecked
 * Lsq computation 
 *   using dgelsy or zgelsy 
 *   we could also use dgelss
 */

static int intdgelsy(NspMatrix *A,NspMatrix *B,NspMatrix **rank,double *rcond);
static int intzgelsy(NspMatrix *A,NspMatrix *B,NspMatrix **rank,double *rcond);

NspMatrix *nsp_lsq(NspMatrix *A,NspMatrix *B)
{
  if (( B =nsp_matrix_copy(B) )== NULLMAT) return NULLMAT;
  if (( A =nsp_matrix_copy(A) )== NULLMAT) return NULLMAT;
  if ( A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r') 
	{
	  /* A real, b real */
	  if ( intdgelsy(A,B,NULL,NULL) == FAIL) return NULLMAT; 
	}
      else 
	{
	  /* A real, b complex */
	  if (nsp_mat_complexify(A,0.0) == FAIL ) return NULLMAT;
	  if ( intzgelsy(A,B,NULL,NULL) == FAIL) return NULLMAT; 
	}
    } 
  else
    {
      if ( B->rc_type == 'r') 
	{
	  /* A complex, B real */
	  if (nsp_mat_complexify(B,0.0) == FAIL ) return NULLMAT;
	  if ( intzgelsy(A,B,NULL,NULL) == FAIL) return NULLMAT; 
	}
      else 
	{
	  /* A complex, b complex */
	  if ( intzgelsy(A,B,NULL,NULL) == FAIL) return NULLMAT; 
	}
    }
  return B;
}


/* A is modified and B too. 
 * result is returned in B which is resized if necessary.
 */

static int intdgelsy(NspMatrix *A,NspMatrix *B,NspMatrix **rank,double *rcond)
{
  double Rcond,eps;
  NspMatrix *jpvt,*dwork;
  int *Ijpvt,irank,info,lworkMin,i; 
  /*  [X,rank]=lsq(A,B,rcond) */
  int m = A->m, n = A->n, mb = B->m,nrhs = B->n ; 

  if (m != mb) {
    Scierror("lsq: first and second arguments must have equal rows\n");
    return FAIL;
  }

  /* A = [] return empty matrices */ 

  if ( A->mn == 0 ) {
    if ( rank != NULL)
      {
	if (( *rank =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }

  /* if rcond is not provided */
  
  eps = C2F(dlamch)("eps", 3L);  
  Rcond = (rcond  == NULL) ? sqrt(eps) : *rcond; 

  /* if n > m then we add empty rows to B */ 
  
  if ( n > m )
    {
      if (nsp_matrix_add_rows(B,n-m) == FAIL) return(FAIL);
    }

  /* jpvt: int matrix */ 
  if (( jpvt =nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return FAIL;
  Ijpvt = (int *) jpvt->R; 
  for (i = 0 ; i < n; ++i) Ijpvt[i]=0;

  /* the min workspace */ 
  lworkMin = Max(Min(m,n) + n*3 + 1,2*Min(m,n) + nrhs);

  if (( dwork =nsp_matrix_create(NVOID,A->rc_type,1,lworkMin)) == NULLMAT) return FAIL;
  C2F(dgelsy)(&m, &n, &nrhs,A->R, &m,B->R,&B->m,Ijpvt,&Rcond,&irank,
	      dwork->R,&lworkMin, &info);
      
  if (info != 0) {
    Scierror("Error: computation failed in dgelsy\n");
    return FAIL;
  }

  nsp_matrix_destroy(jpvt); 
  nsp_matrix_destroy(dwork); 

  if ( n < m) 
    {
      /* we must delete the last rows of B */ 
      if (( dwork =nsp_matrix_create(NVOID,'r',1,m-n)) == NULLMAT) return FAIL;
      for ( i = n+1; i <= m ; i++) dwork->R[i-(n+1)]=i;
      if (nsp_matrix_delete_rows(B,dwork) == FAIL) return FAIL; 
    }
  if ( rank != NULL)
    {
      if (( *rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
      (*rank)->R[0]=irank;
    }

  return OK ;
} 


int intzgelsy(NspMatrix *A,NspMatrix *B,NspMatrix **rank,double *rcond)
{
  double Rcond,eps;
  NspMatrix *jpvt,*dwork,*rwork;
  int *Ijpvt,ix1,ix2,irank,info,lworkMin,i; 
  /*  [X,rank]=lsq(A,B,rcond) */
  int m = A->m, n = A->n, mb = B->m,nrhs = B->n ; 

  if (m != mb) {
    Scierror("Error: first and second arguments in lsq must have equal rows\n");
    return FAIL;
  }

  /* A = [] return empty matrices */ 

  if ( A->mn == 0 ) {
    if ( rank != NULL)
      {
	if (( *rank =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }

  /* if rcond is not provided */
  
  eps = C2F(dlamch)("eps", 3L);  
  Rcond = (rcond  == NULL) ? sqrt(eps) : *rcond; 

  /* if n > m then we add empty rows to B */ 
  
  if ( n > m )
    {
      /* now B->m is Max(m,n) */
      if (nsp_matrix_add_rows(B,n-m) == FAIL) return(FAIL);
    }


  /* jpct: int matrix */ 
  if (( jpvt =nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return FAIL;
  Ijpvt = (int *) jpvt->R; 
  for (i = 0 ; i < n; ++i) Ijpvt[i]=0;

  /* workspace */ 
  
  if (( rwork =nsp_matrix_create(NVOID,'r',1,2*n)) == NULLMAT) return FAIL;

  /* the min workspace */ 

  ix1 = Max(2*Min(m,n),n+1); ix2 = Min(m,n) + nrhs;
  lworkMin = Min(m,n) + Max(ix1,ix2);
  if (( dwork =nsp_matrix_create(NVOID,A->rc_type,1,lworkMin)) == NULLMAT) return FAIL;

  C2F(zgelsy)(&m, &n, &nrhs,A->C, &m, B->C,&B->m,Ijpvt,&Rcond,&irank,
	      dwork->C, &lworkMin, rwork->R, &info);
      
  if (info != 0) {
    Scierror("Error: computation failed in zgelsy\n");
    return FAIL;
  }

  nsp_matrix_destroy(jpvt); 
  nsp_matrix_destroy(dwork); 
  nsp_matrix_destroy(rwork); 

  if ( n < m) 
    {
      /* we must delete the last rows of B */ 
      if (( dwork =nsp_matrix_create(NVOID,'r',1,m-n)) == NULLMAT) return FAIL;
      for ( i = n+1; i <= m ; i++) dwork->R[i-(n+1)]=i;
      if (nsp_matrix_delete_rows(B,dwork) == FAIL) return FAIL; 
    }

  if ( rank != NULL)
    {
      if (( *rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
      (*rank)->R[0]=irank;
    }

  return OK ;
}


/* FIXME: unchecked 
 * interface pour backslash 
 *   A est changée et 
 *   B aussi 
 *   même chose que lsq si ce n'est que si A est carrée inversible on 
 *   utilise une autre méthode. 
 */

static int intdgesv3(NspMatrix *A,NspMatrix *B,NspMatrix **rank,double *rcond,char flag);
static int intzgesv3(NspMatrix *A,NspMatrix *B,NspMatrix **rank,double *rcond,char flag);

int nsp_backslash(NspMatrix *A,NspMatrix *B,NspMatrix **rank,double *rcond,char flag)
{
  if ( A->rc_type == 'r' ) 
    return  intdgesv3(A,B,rank,rcond,flag); 
  else 
    return  intzgesv3(A,B,rank,rcond,flag); 
}


static int intdgesv3(NspMatrix *A,NspMatrix *B,NspMatrix **rank,double *rcond,char flag)
{
  double Rcond=0.0,eps;
  NspMatrix *jpvt,*dwork;
  int *Ijpvt,irank,info,lworkMin,i; 
  /*  [X,rank]=lsq(A,B,rcond) */
  int m = A->m, n = A->n, mb = B->m,nrhs = B->n ; 

  if (m != mb) {
    Scierror("Error: first and second arguments of lsq  must have equal rows\n");
    return FAIL;
  }

  /* A = [] return empty matrices */ 

  if ( A->mn == 0 ) {
    if ( rank != NULL)
      {
	if (( *rank =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }

  /* if rcond is not provided */
  eps = C2F(dlamch)("eps", 3L);  
  Rcond = (rcond  == NULL) ? sqrt(eps) : *rcond; 

  /* if n > m then we add empty rows to B */ 
  
  if ( n > m )
    {
      /* now B->m is Max(m,n) */
      if (nsp_matrix_add_rows(B,n-m) == FAIL) return(FAIL);
    }

  /* jpvt: int matrix */ 
  if (( jpvt =nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return FAIL;
  Ijpvt = (int *) jpvt->R; 

  /* the min workspace */ 
  lworkMin = Max(Min(m,n) + n*3 + 1,2*Min(m,n) + nrhs);
  if (( dwork =nsp_matrix_create(NVOID,A->rc_type,1,lworkMin)) == NULLMAT) return FAIL;

  if ( m == n ) 
    {
      /* Check if the square invertible case is enough  */ 
      double anorm ; 
      NspMatrix *Ac ;
      if (( Ac =nsp_matrix_copy(A))== NULLMAT) return FAIL;
      anorm =  C2F(dlange)("1", &m, &n,Ac->R, &m, dwork->R, 1L);
      C2F(dgetrf)(&n, &n, Ac->R , &n, Ijpvt, &info);
      if (info == 0) {
	C2F(dgecon)("1", &n, Ac->R, &n,&anorm, &Rcond, dwork->R,Ijpvt, &info, 1L);
	if ( Rcond > sqrt(eps)) {
	  /* we solve the linear systems */
	  C2F(dgetrs)("N", &n, &nrhs, Ac->R, &n, Ijpvt,B->R, &n, &info, 1L);
	  /* XXXXXX clean workspace here */ 
	  return OK;
	}
      }
      Sciprintf("Error: Warning, matrix is close to singular or badly scaled. rcond = %e\n, using lsq\n",
	       rcond);
    }
  
  /* we switch to lsq */ 

  for (i = 0 ; i < n; ++i) Ijpvt[i]=0;

  if ( flag == 'n' ) 
    C2F(dgelsy)(&m, &n, &nrhs,A->R, &m,B->R,&B->m,Ijpvt,&Rcond,&irank,
		dwork->R,&lworkMin, &info);
  else 
    {
      /* XXX 
	 C2F(dgelsy1)(&m, &n, &nrhs,A->R, &m,B->R,&B->m,Ijpvt,&Rcond,&irank,
	 dwork->R,&lworkMin, &info);
      */
    }
  if (info != 0) {
    Scierror("Error: computation failed in dgelsy\n");
    return FAIL;
  }

  nsp_matrix_destroy(jpvt); 
  nsp_matrix_destroy(dwork); 

  if ( n < m) 
    {
      /* we must delete the last rows of B */ 
      if (( dwork =nsp_matrix_create(NVOID,'r',1,m-n)) == NULLMAT) return FAIL;
      for ( i = n+1; i <= m ; i++) dwork->R[i-(n+1)]=i;
      if (nsp_matrix_delete_rows(B,dwork) == FAIL) return FAIL; 
      nsp_matrix_destroy(dwork); 
    }

  if ( rank != NULL)
    {
      if (( *rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
      (*rank)->R[0]=irank;
    }

  return OK ;
} 


static int intzgesv3(NspMatrix *A,NspMatrix *B,NspMatrix **rank,double *rcond,char flag)
{
  double Rcond=0.0,eps;
  NspMatrix *jpvt,*dwork,*rwork;
  int *Ijpvt,ix1,ix2,irank,info,lworkMin,i; 
  /*  [X,rank]=lsq(A,B,rcond) */
  int m = A->m, n = A->n, mb = B->m,nrhs = B->n ; 

  if (m != mb) {
    Scierror("Error: first and second arguments of lsq must have equal rows\n");
    return FAIL;
  }

  /* A = [] return empty matrices */ 

  if ( A->mn == 0 ) {
    if ( rank != NULL)
      {
	if (( *rank =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    return OK ; 
  }

  /* if rcond is not provided */
  eps = C2F(dlamch)("eps", 3L);  
  Rcond = (rcond  == NULL) ? sqrt(eps) : *rcond; 

  /* if n > m then we add empty rows to B */ 
  
  if ( n > m )
    {
      /* now B->m is Max(m,n) */
      if (nsp_matrix_add_rows(B,n-m) == FAIL) return(FAIL);
    }


  /* jpvt: int matrix */ 
  if (( jpvt =nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return FAIL;
  Ijpvt = (int *) jpvt->R; 

  /* workspace */ 
  
  if (( rwork =nsp_matrix_create(NVOID,'r',1,2*n)) == NULLMAT) return FAIL;

  /* the min workspace */ 

  ix1 = Max(2*Min(m,n),n+1); ix2 = Min(m,n) + nrhs;
  lworkMin = Min(m,n) + Max(ix1,ix2);
  lworkMin = Max(lworkMin, 2*n); /* for the m==n case */ 

  if (( dwork =nsp_matrix_create(NVOID,A->rc_type,1,lworkMin)) == NULLMAT) return FAIL;


  if ( m == n ) 
    {
      /* Check if the square invertible case is enough  */ 
      double anorm ; 
      NspMatrix *Ac ;
      if (( Ac =nsp_matrix_copy(A))== NULLMAT) return FAIL;
      anorm =  C2F(zlange)("1", &m, &n,Ac->C, &m, NULL , 1L);
      C2F(zgetrf)(&n, &n, Ac->C , &n, Ijpvt, &info);
      if (info == 0) {
	C2F(zgecon)("1", &n, Ac->C, &n,&anorm, &Rcond, dwork->C, rwork->R,&info, 1L);
	if ( Rcond > sqrt(eps)) {
	  /* we solve the linear systems */
	  C2F(zgetrs)("N", &n, &nrhs, Ac->C, &n, Ijpvt,B->C, &n, &info, 1L);
	  /* XXXXXX clean workspace here */ 
	  return OK;
	}
      }
      Sciprintf("solving linear system with square matrix close to singular or badly scaled. rcond = %e\n, using lsq\n",
	       rcond);
    }
  
  for (i = 0 ; i < n; ++i) Ijpvt[i]=0;
  
  if ( flag == 'n' ) 
    C2F(zgelsy)(&m, &n, &nrhs,A->C, &m, B->C,&B->m,Ijpvt,&Rcond,&irank,
		dwork->C, &lworkMin, rwork->R, &info);
  else 
    {
      /* XXX
	 C2F(zgelsy1)(&m, &n, &nrhs,A->C, &m, B->C,&B->m,Ijpvt,&Rcond,&irank,
	 dwork->C, &lworkMin, rwork->R, &info);
      */
      nsp_matrix_destroy(jpvt); 
      nsp_matrix_destroy(dwork); 
      nsp_matrix_destroy(rwork); 
      if ( n < m) 
	{
	  /* we must delete the last rows of B */ 
	  if (( dwork =nsp_matrix_create(NVOID,'r',1,m-n)) == NULLMAT) return FAIL;
	  for ( i = n+1; i <= m ; i++) dwork->R[i-(n+1)]=i;
	  if (nsp_matrix_delete_rows(B,dwork) == FAIL) return FAIL; 
	}

      if ( rank != NULL)
	{
	  if (( *rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;
	  (*rank)->R[0]=irank;
	}

      return OK ;
    }
  return OK;
}

/* FIXME : unchecked 
 * interface pour slash 
 *   On se ramène a backslash après transposition 
 *   A et B sont inchangées 
 */

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


/* FIXME: unchecked 
 * [U,T]=schur(A)  U*T*U' = A
 *     T is returned in A 
 *     U is computed if U != NULL 
 *     F can be null or can be a given function used for reordering 
 *     Sdim is computed if non null 
 * XXX  couvre le cas int C2F(intoschur)(fname, fname_len)
 *------------------------------*/

int intdgees0(NspMatrix *A,NspMatrix **U,int (*F)(double *re,double *im), NspMatrix **Sdim) 
{
  NspMatrix *wr,*wi,*dwork,*iwork;
  int info,lworkMin,sdim; 
  int m = A->m, n = A->n, *Iwork = NULL;
  char *sort = "N";
  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ( U != NULL) {
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
    }
    if ( Sdim != NULL) {
      if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;   
      (*Sdim)->R[0] = 0;
    }
    return OK ; 
  }
  
  if (m != n) { 


    Scierror("Error: first argument of schur should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  if (( wr =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;
  if (( wi =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;

  lworkMin = n * 3;

  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;   
  
  if ( F != NULL ) 
    {
      if (( iwork =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;   
      Iwork = (int *) iwork->R;
      sort = "S";
    }

  if ( U != NULL ) 
    {
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
      C2F(dgees)("V",sort, F, &n,A->R,&n, &sdim,wr->R, wi->R,(*U)->R,
		 &n, dwork->R, &lworkMin, Iwork, &info, 4L, 4L);
    }
  else 
    { 
      C2F(dgees)("N",sort, F, &n,A->R,&n, &sdim,wr->R, wi->R,NULL,
		 &n, dwork->R, &lworkMin, Iwork, &info, 4L, 4L);
    }

  if ( Sdim != NULL ) {
    if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;   
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
    return FAIL;
  }
  /* menage XXXX */ 

  return OK ;
}

/* 
 * XXX  couvre lecas int C2F(intzschur)(fname, fname_len)
 */


int intzgees0(NspMatrix *A,NspMatrix **U,int (*F)(doubleC *w), NspMatrix **Sdim) 
{
  int *Iwork = NULL;
  NspMatrix *W,*dwork,*rwork,*iwork;
  int info,lworkMin,sdim; 
  int m = A->m, n = A->n;
  char *sort = "N";
  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ( U != NULL) {
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
    }
    if ( Sdim != NULL) {
      if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;   
      (*Sdim)->R[0] = 0;
    }
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of schur should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  if (( W =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT) return FAIL;

  lworkMin = n * 2;
  if (( dwork =nsp_matrix_create(NVOID,'c',1,lworkMin)) == NULLMAT) return FAIL;   
  
  if ( F != NULL ) 
    {
      if (( iwork =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;   
      Iwork = (int *) iwork->R;
      sort = "S";
    }

  if ( U != NULL ) 
    {
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
      C2F(zgees)("V",sort, F, &n,A->C,&n, &sdim,W->C,(*U)->C,
		 &n, dwork->C, &lworkMin,rwork->R, Iwork, &info, 4L, 4L);
    }
  else 
    { 
      C2F(zgees)("N",sort, F, &n,A->C,&n, &sdim,W->C,NULL,
		 &n, dwork->C, &lworkMin,rwork->R, Iwork, &info, 4L, 4L);
    }
  if (info > 0) {
    Scierror("Error: in schur, the QR algorithm failed to compute all the eigenvalues;\n");
    return FAIL;
  }

  /* menage XXXX */ 

  return OK ;
}


/*
 *   [U,T]=schur(A, 'r' | 'c' ) 
 *   like intdgees0 but matrix A can be complexified if 
 *   requested 
 */

int intdgees1(NspMatrix *A,NspMatrix **U,char flag)
{
  if ( flag == 'r' ) 
    return  intdgees0(A,U,NULL,NULL);
  else   if ( flag == 'c' ) 
    {
      if (nsp_mat_complexify(A,0.0) == FAIL ) return FAIL;
      return  intzgees0(A,U,NULL,NULL);
    }
  else 
    {
      Scierror("Error: wrong flag '%c' in intdgees1\n",flag);
      return FAIL;
    }
  return OK;
}


int intzgees1(NspMatrix *A,NspMatrix **U,char flag)
{
  if ( flag == 'c' ) 
    {
      return  intzgees0(A,U,NULL,NULL);
    }
  else 
    {
      Scierror("Error: wrong flag '%c' in intzgees1\n",flag);
      return FAIL;
    }
  return OK;
}

/*--------------------------------------------
 * A and B are overwriten by their schur form 
 *--------------------------------------------*/

int intdgges(NspMatrix *A,NspMatrix *B,
	     int (*F)(double *alphar,double *alphai,double *beta),
	     NspMatrix **VSL,NspMatrix **VSR,NspMatrix **Sdim) 
{
  double  *Vsl,*Vsr;
  NspMatrix *alphar,*alphai,*beta,*dwork,*iwork;
  int info,lworkMin,sdim,*Iwork; 
  int m = A->m, n = A->n, mb = B->m,nb = B->n ;  
  char *sort = "N",*jobvsl="N",*jobvsr= "N";

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ( VSR != NULL) {
      if (( *VSR =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
    }
    if ( VSL != NULL) {
      if (( *VSR =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
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

  lworkMin = (n+1) * 7+16;
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;   
  
  if ( F != NULL ) 
    {
      if (( iwork =nsp_matrix_create(NVOID,'r',2*n,1)) == NULLMAT) return FAIL;   
      Iwork = (int *) iwork->R;
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

  C2F(dgges)(jobvsl, jobvsr, sort,F, &n, A->R, &n, B->R, &n, 
	     &sdim,alphar->R, alphai->R, beta->R,Vsl, &n,Vsr, &n,
	     dwork->R, &lworkMin, Iwork, &info, 1L, 1L, 1L);
  
  if ( Sdim != NULL ) {
    if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;   
    (*Sdim)->R[0] = sdim;
  }

  if (info > 0) {
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
  /* menage XXXX */ 

  return OK ;
}


int intzgges(NspMatrix *A,NspMatrix *B,
	     int (*F)(doubleC *alpha,doubleC *beta),
	     NspMatrix **VSL,NspMatrix **VSR,NspMatrix **Sdim) 
{
  doubleC  *Vsl,*Vsr;
  NspMatrix *alpha,*beta,*dwork,*iwork,*rwork;
  int info,lworkMin,sdim,*Iwork; 
  int m = A->m, n = A->n, mb = B->m,nb = B->n ;  
  char *sort = "N",*jobvsl="N",*jobvsr= "N";

  /*  A = [] return empty matrices */ 
  
  if ( A->mn == 0 ) {
    if ( VSR != NULL) {
      if (( *VSR =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
    }
    if ( VSL != NULL) {
      if (( *VSR =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
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
  lworkMin = 2*n;
  if (( dwork =nsp_matrix_create(NVOID,'c',1,lworkMin)) == NULLMAT) return FAIL;   
  
  if ( F != NULL ) 
    {
      if (( iwork =nsp_matrix_create(NVOID,'r',2*n,1)) == NULLMAT) return FAIL;   
      Iwork = (int *) iwork->R;
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
      jobvsl = "V"; 
    }

  C2F(zgges)(jobvsl, jobvsr, sort,F, &n, A->C, &n, B->C, &n, 
	     &sdim,alpha->C, beta->C,Vsl, &n,Vsr, &n,
	     dwork->C, &lworkMin,rwork->R,Iwork, &info, 1L, 1L, 1L);
  
  if ( Sdim != NULL ) {
    if (( *Sdim =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return FAIL;   
    (*Sdim)->R[0] = sdim;
  }

  if (info > 0) {
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
  if ( A->rc_type == 'r') 
    return intdgehrd(A,U);
  else 
    return intzgehrd(A,U);
}

static int intdgehrd(NspMatrix *A, NspMatrix **U)
{
  NspMatrix *tau,*dwork;
  int workMin, info, m = A->m,n=A->n,un=1;

  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  {
    if ( U != NULL) {
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    }
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of hess should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  if (( tau =nsp_matrix_create(NVOID,'r',1,n-1)) == NULLMAT) return FAIL;
  workMin = Max(1,n);
  if (( dwork =nsp_matrix_create(NVOID,'r',1,workMin)) == NULLMAT) return FAIL;

  C2F(dgehrd)(&n, &un, &n,A->R, &n,tau->R,dwork->R, &workMin, &info);
  if (info != 0) {
    Scierror("Error: something wrong in dgehrd\n");
    return FAIL;
  }

  if ( U != NULL) { 
    /* extract U */
    if (( *U =nsp_matrix_copy(A))== NULLMAT) return FAIL;
    C2F(dorghr)(&n,&un,&n,(*U)->R,&n,tau->R,dwork->R,&workMin,&info);    
    if (info != 0) {
      Scierror("Error: something wrong in dorghr\n");
      return FAIL;
    }
  }
  /* extract H */ 
  nsp_mat_triu(A,-1);    
  return OK;
} 


static int intzgehrd(NspMatrix *A, NspMatrix **U)
{
  NspMatrix *tau,*dwork;
  int  workMin, info, m = A->m,n=A->n,un=1;

  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  {
    if ( U != NULL) {
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    }
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of hess should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  if (( tau =nsp_matrix_create(NVOID,'c',1,n-1)) == NULLMAT) return FAIL;
  workMin = Max(1,n);
  if (( dwork =nsp_matrix_create(NVOID,'c',1,workMin)) == NULLMAT) return FAIL;

  C2F(zgehrd)(&n, &un, &n,A->C, &n,tau->C,dwork->C, &workMin, &info);
  if (info != 0) {
    Scierror("Error: something wrong in zgehrd\n");
    return FAIL;
  }

  if ( U != NULL) { 
    /* extract U */
    if (( *U =nsp_matrix_copy(A))== NULLMAT) return FAIL;
    C2F(zunghr)(&n,&un,&n,(*U)->C,&n,tau->C,dwork->C,&workMin,&info);    
    if (info != 0) {
      Scierror("Error: something wrong in zunghr\n");
      return FAIL;
    }
  }
  /* extract H */ 
  nsp_mat_triu(A,-1);    
  return OK;
} 


/* FIXME 
 * [al,be,Q,Z] = gspec(A,E) returns in addition the matrix Q and Z
 * of generalized left and right eigenvectors of the pencil.
 * Vl = Q , Vr = Z
 */

static int intdggev(NspMatrix *A,NspMatrix *B,NspMatrix **Vl,NspMatrix **Vr,NspMatrix **alpha,NspMatrix **beta);
static int intzggev(NspMatrix *A,NspMatrix *B,NspMatrix **Vl,NspMatrix **Vr,NspMatrix **alpha,NspMatrix **beta);

int idl_gspec(NspMatrix *A,NspMatrix *B,NspMatrix **Vl,NspMatrix **Vr,NspMatrix **alpha,NspMatrix **beta)
{
  if (( B =nsp_matrix_copy(B) )== NULLMAT) return FAIL;
  if (( A =nsp_matrix_copy(A) )== NULLMAT) return FAIL;
  if ( A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r') 
	{
	  /* A real, b real */
	  if ( intdggev(A,B,Vl,Vr,alpha,beta) == FAIL) return FAIL; 
	}
      else 
	{
	  /* A real, b complex */
	  if (nsp_mat_complexify(A,0.0) == FAIL ) return FAIL;
	  if ( intzggev(A,B,Vl,Vr,alpha,beta) == FAIL) return FAIL; 
	}
    } 
  else
    {
      if ( B->rc_type == 'r') 
	{
	  /* A complex, B real */
	  if (nsp_mat_complexify(B,0.0) == FAIL ) return FAIL;
	  if ( intzggev(A,B,Vl,Vr,alpha,beta) == FAIL) return FAIL; 
	}
      else 
	{
	  /* A complex, b complex */
	  if ( intzggev(A,B,Vl,Vr,alpha,beta) == FAIL) return FAIL; 
	}
    }
  return OK;
}



static int intdggev(NspMatrix *A,NspMatrix *B,NspMatrix **Vl,NspMatrix **Vr,NspMatrix **alpha,NspMatrix **beta)
{
  double *VrR=NULL,*VlR=NULL; 
  char *jobVl ="N", *jobVr = "N",type = 'r';
  NspMatrix *alphar,*alphai,*dwork; 
  int info,lworkMin,i,j;  

  int m = A->m, n = A->n, mb = B->m,nb = B->n ;  

  /* A = [] return empty matrices */ 

  if ( A->mn == 0 ) {
    if ( Vl != NULL)
      {
	if (( *Vl =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    if ( Vr != NULL)
      {
	if (( *Vr =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    if (( *alpha =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if (( *beta =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of spec should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  if (mb != nb) { 
    Scierror("Error: second argument of spec should be square and it is (%dx%d)\n", 
	     mb,nb);
    return FAIL;
  }

  if (m != mb || n != nb ) {
    Scierror("spec: first and second arguments must have equal size\n");
    return FAIL;
  }

  /* XXX Check that A and B are finite matrices */ 
  
  if (( *beta =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;

  if (( alphar =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;
  if (( alphai =nsp_matrix_create(NVOID,'r',n,1)) == NULLMAT) return FAIL;

  if ( Vl != NULL ) {
    if (( *Vl =nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT) return FAIL;
    VlR = (*Vl)->R; jobVl = "V";
  }
  if ( Vr != NULL ) {
    if (( *Vr =nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT) return FAIL;
    VrR = (*Vr)->R; jobVr = "V";
  }

  lworkMin = Max(1,8*n);
  if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;

  C2F(dggev)(jobVl,jobVr,&n,A->R, &n,B->R, &n, alphar->R, alphai->R,(*beta)->R,
	     VlR,&n,VrR, &n,dwork->R,&lworkMin, &info, 1L, 1L);

  if (info != 0) {
    if (info <= n && info > 0 ) 
      Scierror("Error: spec, the QZ iteration failed.  No eigenvectors have been calculated\n");
    else if (info == n + 1) 
      Scierror("Error: spec, other than QZ iteration failed in dhgeqz\n");
    else if (info == n + 2) 
      Scierror("Error: spec, error return from DTGEVC\n");
    else 
      Scierror("Error: spec, wrong argument (%d) in dggev call\n",-info);
    return FAIL;
  }

  /* return (alphar + %i*alphai) and beta */
  /* result is real ? or complex */ 
  
  for (i = 0 ; i < n ; ++i) { if (alphai->R[i] != 0.0) { type = 'c'; break;}}

  /* alpha = (alphar,alphai) */ 

  if ( type == 'r' ) 
    { *alpha = alphar; }
  else 
    { 
      if (( *alpha =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT) return FAIL;
      for (i = 0 ; i < n ; ++i) { (*alpha)->C[i].r = alphar->R[i]; (*alpha)->C[i].i = alphai->R[i]; }
    }

  /* fill Vr if requested */ 

  if ( Vr != NULL && type == 'c' ) {
    if (nsp_mat_complexify(*Vr,0.0) == FAIL ) return FAIL;
    j = -1;
    while (1) {
      j ++ ; 
      if( alphai->R[j] != 0.0 ) {
	for ( i = 0;  i < n ; ++i) 
	  { 
	    int k = i+j*n ;
	    (*Vr)->C[k].i = (*Vr)->C[k+n].r; 
	    (*Vr)->C[k+n].r = (*Vr)->C[k].r ;
	    (*Vr)->C[k+n].i = - (*Vr)->C[k].i;
	  }
	j++; 
      }
      if (j >= n) break; 
    }
  }
  
  /* fill Vl if requested */ 

  if ( Vl != NULL && type == 'c' ) {
    if (nsp_mat_complexify(*Vl,0.0) == FAIL ) return FAIL;
    j = -1;
    while (1) {
      j ++ ; 
      if( alphai->R[j] != 0.0 ) {
	for ( i = 0;  i < n ; ++i) 
	  { 
	    int k = i+j*n ;
	    (*Vl)->C[k].i = (*Vl)->C[k+n].r; 
	    (*Vl)->C[k+n].r = (*Vl)->C[k].r ;
	    (*Vl)->C[k+n].i = - (*Vl)->C[k].i;
	  }
	j++; 
      }
      if (j >= n) break; 
    }
  }

  /* un peu de ménage XXXXX */ 

  return OK ; 
} 


static int intzggev(NspMatrix *A,NspMatrix *B,NspMatrix **Vl,NspMatrix **Vr,NspMatrix **alpha,NspMatrix **beta)
{
  doubleC *VrI=NULL,*VlI=NULL; 
  char *jobVl ="N", *jobVr = "N";
  NspMatrix *rwork,*dwork; 
  int info,lworkMin;  

  int m = A->m, n = A->n, mb = B->m,nb = B->n ;  

  /* A = [] return empty matrices */ 

  if ( A->mn == 0 ) {
    if ( Vl != NULL)
      {
	if (( *Vl =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    if ( Vr != NULL)
      {
	if (( *Vr =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      }
    if (( *alpha =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    if (( *beta =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
    return OK ; 
  }
  
  if (m != n) { 
    Scierror("Error: first argument of spec should be square and it is (%dx%d)\n", 
	     m,n);
    return FAIL;
  }

  if (mb != nb) { 
    Scierror("Error: second argument of spec should be square and it is (%dx%d)\n", 
	     mb,nb);
    return FAIL;
  }

  if (m != mb || n != nb ) {
    Scierror("Error: spec, first and second arguments must have equal size\n");
    return FAIL;
  }

  /* XXX Check that A and B are finite matrices */ 


  if (( *alpha =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT) return FAIL;  
  if (( *beta =nsp_matrix_create(NVOID,'c',n,1)) == NULLMAT) return FAIL;

  if ( Vl != NULL ) {
    if (( *Vl =nsp_matrix_create(NVOID,'c',m,n)) == NULLMAT) return FAIL;
    VlI = (*Vl)->C; jobVl = "V";
  }
  if ( Vr != NULL ) {
    if (( *Vr =nsp_matrix_create(NVOID,'c',m,n)) == NULLMAT) return FAIL;
    VrI = (*Vr)->C; jobVr = "V";
  }

  if (( rwork =nsp_matrix_create(NVOID,'r',1,8*n)) == NULLMAT) return FAIL;
  lworkMin = Max(1,8*n);
  if (( dwork =nsp_matrix_create(NVOID,'c',1,lworkMin)) == NULLMAT) return FAIL;

  C2F(zggev)(jobVl,jobVr,&n,A->C, &n,B->C, &n,(*alpha)->C,(*beta)->C,
	     VlI,&n,VrI, &n,dwork->C,&lworkMin,rwork->R,&info, 1L, 1L);

  if (info != 0) {
    if (info <= n && info > 0 ) 
      Scierror("Error: spec, The QZ iteration failed.  No eigenvectors have been calculated\n");
    else if (info == n + 1) 
      Scierror("Error: spec, other than QZ iteration failed in dhgeqz\n");
    else if (info == n + 2) 
      Scierror("Error: spec, error return from DTGEVC\n");
    else 
      Scierror("Error: spec, wrong argument (%d) in dggev call\n",-info);
    return FAIL;
  }

  /* un peu de ménage XXXXX */ 

  return OK ; 
} 


  
/* nsp_norm 
 *   norm(A,'xxx') 
 *   'M' '1' 'I' 'F' 
 */ 

static double intznorm(NspMatrix *A,char flag);
static double intdnorm(NspMatrix *A,char flag);

double nsp_norm(NspMatrix *A,char flag) 
{
  if ( A->rc_type == 'r' ) 
    return intdnorm(A,flag) ;
  else 
    return intznorm(A,flag) ;
}

static double intdnorm(NspMatrix *A,char flag)
{
  double norm;
  NspMatrix *dwork;
  int lworkMin, m = A->m, n = A->n ;
  /*  A = [] return 0 */ 
  if ( A->mn == 0 ) return 0.0;
  if ( flag == 'I') 
    {
      lworkMin = Max(1,n);
      if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;
      norm=  C2F(dlange)(&flag, &m, &n,A->R, &m, dwork->R, 1L);
      nsp_matrix_destroy(dwork);
    } 
  else 
    {
      norm=  C2F(dlange)(&flag, &m, &n,A->R, &m, NULL , 1L);
    }
  return norm;
} 

static double intznorm(NspMatrix *A,char flag)
{
  double norm;
  NspMatrix *dwork;
  int lworkMin, m = A->m, n = A->n ;
  /*  A = [] return empty matrices */ 
  if ( A->mn == 0 ) return 0.0;

  if ( flag == 'I') 
    {
      lworkMin = Max(1,n);
      if (( dwork =nsp_matrix_create(NVOID,'r',1,lworkMin)) == NULLMAT) return FAIL;
      norm=  C2F(zlange)(&flag, &m, &n,A->C, &m, dwork->R, 1L);
      nsp_matrix_destroy(dwork);
    } 
  else 
    {
      norm=  C2F(zlange)(&flag, &m, &n,A->C, &m, NULL , 1L);
    }
  return norm;
}
