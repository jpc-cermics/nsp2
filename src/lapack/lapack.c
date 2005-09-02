/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2005 Bruno Pincon Esial/Iecn
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
 *   FIXME : use dsyevr / zheevr for spec_sym ?
 */

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "nsp/interf.h"
#include "nsp/lapack-c.h"
#include "nsp/cnumeric.h"
#include "nsp/blas.h"
#include "nsp/nsp_lapack.h"


/**
 * nsp_mat_is_symmetric:
 * @A: a #NspMatrix
 * 
 * test if the matrix @A is symetric (real case) or hermitian (complex case)
 * 
 * Return value: TRUE or FALSE
 **/
int nsp_mat_is_symmetric(NspMatrix *A)
{
  int i,j;
  if ( A->m != A->n ) return FALSE;
  if ( A->rc_type == 'r') 
    {
      for ( i=0 ; i < A->m ; i++)
	for ( j=0 ; j < i ; j++)
	  {
	    double dx= Abs(A->R[i+j*A->m] - A->R[j+i*A->m]);
	    if ( A->R[i+j*A->m] + dx > A->R[i+j*A->m]) 
	      return FALSE;
	  }
    }
  else 
    {
      for ( i=0 ; i < A->m ; i++)
	for ( j=0 ; j < i ; j++)
	  {
	    double dxr= Abs(A->C[i+j*A->m].r - A->C[j+i*A->m].r);
	    double dxi= Abs(A->C[i+j*A->m].i + A->C[j+i*A->m].i);
	    if ( A->C[i+j*A->m].r + dxr > A->C[i+j*A->m].r) return FALSE;
	    if ( A->C[i+j*A->m].i + dxi > A->C[i+j*A->m].i) return FALSE;
	  }
    }
  return TRUE;
}

int nsp_mat_is_lower_triangular(NspMatrix *A)
{
  int i,j;
  if ( A->m != A->n ) return FALSE;
  if ( A->rc_type == 'r') 
    {
      for ( j=0 ; j < A->m ; j++)
	for ( i=0 ; i < j ; i++)
	  if ( A->R[i+j*A->m] != 0.0 ) 
	    return FALSE;
    }
  else 
    {
      for ( j=0 ; j < A->m ; j++)
	for ( i=0 ; i < j ; i++)
	  if ( A->C[i+j*A->m].r != 0.0  ||  A->C[i+j*A->m].i != 0.0 ) 
	    return FALSE;
    }
  return TRUE;
}

int nsp_mat_is_upper_triangular(NspMatrix *A)
{
  int i,j;
  if ( A->m != A->n ) return FALSE;
  if ( A->rc_type == 'r') 
    {
      for ( j=0 ; j < A->m ; j++)
	for ( i=j+1 ; i < A->m ; i++)
	  if ( A->R[i+j*A->m] != 0.0 ) 
	    return FALSE;
    }
  else 
    {
      for ( j=0 ; j < A->m ; j++)
	for ( i=j+1 ; i < A->m ; i++)
	  if ( A->C[i+j*A->m].r != 0.0  ||  A->C[i+j*A->m].i != 0.0 ) 
	    return FALSE;
    }
  return TRUE;
}

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
		      NspMatrix **Rank, NspMatrix **Sval, double *tol,char flag);
static int intzgeqrpf(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E,
		      NspMatrix **Rank, NspMatrix **Sval, double *tol,char flag);

/* OK 
 * QR decomposition  
 * mode can be 'x' or 'e' (economic) 
 * rank and E are computed if non null arguments are transmited 
 * tol can be given or computed if null is transmited 
 */

int nsp_qr(NspMatrix *A,NspMatrix **Q,NspMatrix **R,NspMatrix **E, NspMatrix **Rank, 
	   NspMatrix **Sval, double *tol,char mode)
{
  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  {
    if (( *Q =nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT) return FAIL;
    if (( *R =nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT) return FAIL;
    if ( E != NULL)
      {
	if ( (*E =nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT) return FAIL;
      }
    if (Rank != NULL ) 
    {
      if ( (*Rank =nsp_matrix_create(NVOID,'r',A->m,A->n)) == NULLMAT ) return FAIL;
    }
    if (Sval != NULL ) 
    {
      if ( (*Sval =nsp_matrix_create(NVOID,'r',A->m,A->n)) == NULLMAT ) return FAIL;
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
  int lwork, info, *jpvt=NULL, m=A->m, n=A->n, Minmn=Min(m,n);

  if ( (r=nsp_matrix_copy(A)) == NULLMAT ) return FAIL ; 

  if ( (tau=nsp_alloc_work_doubles(Minmn)) == NULL ) goto err;

  lwork = -1;

  if ( E == NULL ) 
    {
      C2F(dgeqrf)(&m, &n, r->R, &m, tau, qwork, &lwork, &info);
      lwork = (int) qwork[0];
      if ( (work=nsp_alloc_work_doubles(lwork)) == NULL ) goto err;
      C2F(dgeqrf)(&m, &n, r->R, &m, tau, work, &lwork, &info);
    } 
  else 
    {
      if ( (jpvt=nsp_alloc_work_int(n)) == NULL ) goto err;
      for (i = 0; i < n ; i++) jpvt[i]= 0;
      C2F(dgeqp3)(&m, &n, r->R, &m, jpvt, tau, qwork, &lwork, &info);
      lwork = (int) qwork[0];
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
  int lwork, info, *jpvt=NULL, m=A->m, n=A->n, Minmn=Min(m,n);

  if ( (r=nsp_matrix_copy(A)) == NULLMAT ) return FAIL ; 

  if ( (tau=nsp_alloc_work_doubleC(Minmn)) == NULL ) goto err;

  lwork = -1;

  if ( E == NULL ) 
    {
      C2F(zgeqrf)(&m, &n, r->C, &m, tau, qwork, &lwork, &info);
      lwork = (int) qwork[0].r;
      if ( (cwork=nsp_alloc_work_doubleC(lwork)) == NULL ) goto err;
      C2F(zgeqrf)(&m, &n, r->C, &m, tau, cwork, &lwork, &info);
    } 
  else 
    {
      if ( (rwork=nsp_alloc_work_doubles(2*n)) == NULL ) goto err;
      if ( (jpvt=nsp_alloc_work_int(n)) == NULL ) goto err;
      for (i = 0; i < n ; i++) jpvt[i]= 0;
      C2F(zgeqp3)(&m, &n, r->C, &m, jpvt, tau, qwork, &lwork, rwork, &info);
      lwork = (int) qwork[0].r;
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


/*  
 * Lu factorization 
 * A is changed by nsp_lu. Modified by Bruno (June 21 2005) so
 * as to lowering the memory requirement :
 *      - use A to store L
 *      - the permutation E is returned as a permutation and not
 *        as a permutation matrix (precisely E is the inverse 
 *        permutation of the one associated to permutation matrix
 *        P in PA = LU). This brings some speedup in solving
 *        linear systems with:
 *
 *          x = U\(L\b(E,:))
 * 
 *        In place of using:  x = U\(L\(P*b))
 */
int intdgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **E);
int intzgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **E);

int nsp_lu(NspMatrix *A,NspMatrix **L,NspMatrix **E)
{
  /* A == [] return empty matrices*/ 
  if ( A->mn == 0 )  
    {
      if ( (*L=nsp_matrix_create(NVOID,A->rc_type,0,0)) == NULLMAT ) return FAIL;
      if ( E != NULL )
	if ( (*E=nsp_matrix_create(NVOID,A->rc_type,0,0)) == NULLMAT ) return FAIL;
      return OK ; 
    }

  if (A->rc_type == 'r' ) 
    return  intdgetrf(A,L,E);
  else 
    return  intzgetrf(A,L,E);
}

int intdgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **E)
{
  int info, m=A->m, n=A->n , Minmn, i, j, k, ind, nbsd;
  int *ipiv=NULL, *invp=NULL;
  double zeroD=0.0, oneD=1.0;

   Minmn = Min(m,n);

  if ( (*L=nsp_matrix_create(NVOID,A->rc_type,m,Minmn)) == NULLMAT ) return FAIL;
 
  if ( E != NULL )
    if ( (*E=nsp_matrix_create(NVOID,'r',m,1)) == NULLMAT ) return FAIL;
  if ( (ipiv = nsp_alloc_work_int(Minmn)) == NULL ) return FAIL;
  if ( (invp = nsp_alloc_work_int(m)) == NULL ) { FREE(ipiv); return FAIL;}

  C2F(dgetrf)(&m, &n, A->R, &m, ipiv, &info);
  
  /* compute the inverse permutation invpiv */ 
  for ( i=1 ; i <= m ; i++) invp[i-1]=i;
  for ( i=1 ; i <= Minmn ; i++) 
    {
      int ip = ipiv[i-1];
      if ( ip != i) { int iw = invp[i-1]; invp[i-1] = invp[ip-1]; invp[ip-1] = iw;}
    }

  /* fill L with the lower triangular part of A */
  if ( E != NULL )
    {
      for ( i = 0 ; i < m ; i++ ) (*E)->R[i] = (double) invp[i];
      C2F(dlaset)("A", &m, &Minmn, &zeroD, &oneD, (*L)->R, &m, 1L);
      for ( j = 0 ; j < Minmn ; j++ )
	{
	  ind = (m+1)*j+1; nbsd = m-j-1;
	  if ( nbsd > 0 )
	    memcpy(&((*L)->R[ind]), &(A->R[ind]), nbsd*sizeof(double));
	  for ( k = 0 ; k < nbsd ; k++ ) A->R[ind+k] = 0.0; 	   
	} 
    }
  else  /* but if the permutation is not required we must return P^(-1) L  */
    {
      for ( i = 0 ; i < Minmn ; i++ )
	{
	  k = invp[i] - 1;  /* caution invp is 1-based */ 
	  for ( j = 0 ; j < i ; j++ )
	    { (*L)->R[k+j*m] = A->R[i+j*m];  A->R[i+j*m] = 0.0;}
	  (*L)->R[k+i*m] = 1.0;
	  for ( j = i+1 ; j < Minmn ; j++ )
	    (*L)->R[k+j*m] = 0.0;
	}
      for ( i = Minmn ; i < m ; i++ )
	{
	  k = invp[i] - 1;  /* caution invp is 1-based */ 
	  for ( j = 0 ; j < Minmn ; j++ )
	    { (*L)->R[k+j*m] = A->R[i+j*m];  A->R[i+j*m] = 0.0;}
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
  return OK;
}

int intzgetrf(NspMatrix *A,NspMatrix **L,NspMatrix **E)
{
  int info, m=A->m, n=A->n , Minmn, i, j, k, ind, nbsd;
  int *ipiv=NULL, *invp=NULL;
  doubleC zeroC={0.0,0.0}, oneC={1.0,0.0};

  Minmn = Min(m,n);

  if ( (*L=nsp_matrix_create(NVOID,A->rc_type,m,Minmn)) == NULLMAT ) return FAIL;
 
  if ( E != NULL )
    if ( (*E=nsp_matrix_create(NVOID,'r',m,1)) == NULLMAT ) return FAIL;
  if ( (ipiv = nsp_alloc_work_int(Minmn)) == NULL ) return FAIL;
  if ( (invp = nsp_alloc_work_int(m)) == NULL ) { FREE(ipiv); return FAIL;}

  C2F(zgetrf)(&m, &n, A->C, &m, ipiv, &info);
  
  /* compute the inverse permutation invpiv */ 
  for ( i=1 ; i <= m ; i++) invp[i-1]=i;
  for ( i=1 ; i <= Minmn ; i++) 
    {
      int ip = ipiv[i-1];
      if ( ip != i) { int iw = invp[i-1]; invp[i-1] = invp[ip-1]; invp[ip-1] = iw;}
    }

  /* fill L with the inf triangular part of A */
  if ( E != NULL )
    {
      for ( i = 0 ; i < m ; i++ ) (*E)->R[i] = (double) invp[i];
      C2F(zlaset)("A", &m, &Minmn, &zeroC, &oneC, (*L)->C, &m, 1L);
      for ( j = 0 ; j < Minmn ; j++ )
	{
	  ind = (m+1)*j+1; nbsd = m-j-1;
	  if ( nbsd > 0 )
	    memcpy(&((*L)->C[ind]), &(A->C[ind]), nbsd*sizeof(doubleC));
	  for ( k = 0 ; k < nbsd ; k++ ) A->C[ind+k] = zeroC; 	   
	} 
    }
  else  /* but if the permutation is not required we must return P^(-1) L  */
    {
      for ( i = 0 ; i < Minmn ; i++ )
	{
	  k = invp[i] - 1;  /* invp is 1-based */ 
	  for ( j = 0 ; j < i ; j++ )
	    { (*L)->C[k+j*m] = A->C[i+j*m];  A->C[i+j*m] = zeroC;}
	  (*L)->C[k+i*m] = oneC;
	  for ( j = i+1 ; j < Minmn ; j++ )
	    (*L)->C[k+j*m] = zeroC;
	}
      for ( i = Minmn ; i < m ; i++ )
	{
	  k = invp[i] - 1;  /* invp is 1-based */ 
	  for ( j = 0 ; j < Minmn ; j++ )
	    { (*L)->C[k+j*m] = A->C[i+j*m];  A->C[i+j*m] = zeroC;}
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
static int intdgesdd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol);
static int intzgesdd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol);

int nsp_svd(NspMatrix *A,NspMatrix **S,NspMatrix **U,NspMatrix **V,char flag,NspMatrix **Rank,double *tol)
{
/*   if ( (A=nsp_matrix_copy(A)) == NULLMAT ) return FAIL; */

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
  int m = A->m, n=A->n, lwork, info, i, Minmn = Min(m,n), *iwork=NULL;
  NspMatrix *u=NULLMAT, *s=NULLMAT, *vt=NULLMAT, *v=NULLMAT;
  double *dwork=NULL, qwork[1]; 

  /* checks that  A != Nan et != Inf */
  for ( i= 0 ; i < A->mn ; i++ ) 
    if (isinf (A->R[i]) || isnan (A->R[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

  if ( (s=nsp_matrix_create(NVOID,'r',Minmn,1)) == NULLMAT ) return FAIL;

  if ( (iwork=nsp_alloc_work_int(8*Minmn)) == NULL ) goto err;
  lwork = -1;
  if ( U == NULL ) /* just compute the singular values */  
    {
      C2F(dgesdd)("N", &m, &n, A->R, &m, s->R, NULL, &m, NULL, &n, qwork, &lwork, iwork, &info, 1L);
      lwork = (int) qwork[0];
      if ( (dwork=nsp_alloc_work_doubles(lwork)) == NULL ) goto err;
      C2F(dgesdd)("N", &m, &n, A->R, &m, s->R, NULL, &m, NULL, &n, dwork, &lwork, iwork, &info, 1L);
    } 
  else             /* we also compute U and Vt */ 
    {
      int nU  = ( flag == 'e') ? Minmn : m;
      int mVt = ( flag == 'e') ? Minmn : n;
      if ( (u=nsp_matrix_create(NVOID,'r',m,nU)) == NULLMAT ) goto err;
      if ( (vt=nsp_matrix_create(NVOID,'r',mVt,n)) == NULLMAT ) goto err;
      C2F(dgesdd)(flag=='e'?"S":"A", &m, &n, A->R, &m, s->R, u->R, &m, vt->R, &mVt, qwork, &lwork, iwork, &info, 1L);
      lwork = (int) qwork[0];
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
  int m = A->m, n=A->n, lwork, info, i, Minmn = Min(m,n), *iwork=NULL;
  NspMatrix *u=NULLMAT, *s=NULLMAT, *vt=NULLMAT, *v=NULLMAT;
  doubleC *cwork=NULL, qwork[1]; 
  double *rwork=NULL;

  /* checks that  A != Nan et != Inf */
  for ( i= 0 ; i < A->mn ; i++ ) 
    if (isinf (A->R[i]) || isnan (A->R[i]))
      {
	Scierror("Error: nan or inf in svd first argument\n"); 
	return FAIL;
      }

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
      int nU  = ( flag == 'e') ? Minmn : m;
      int mVt = ( flag == 'e') ? Minmn : n;
      if ( (u=nsp_matrix_create(NVOID,'c',m,nU)) == NULLMAT ) goto err;
      if ( (vt=nsp_matrix_create(NVOID,'c',mVt,n)) == NULLMAT ) goto err;
      if ( (rwork=nsp_alloc_work_doubles(5*Minmn*Minmn+5*Minmn)) == NULL ) goto err;
      C2F(zgesdd)(flag=='e'?"S":"A", &m, &n, A->C, &m, s->R, u->C, &m, vt->C, &mVt, qwork, &lwork, rwork, iwork, &info, 1L);
      lwork = (int) qwork[0].r;
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



/* OK
 * nsp_spec 
 * spec(A) for nonsymmetric matrices using dgeev or zgeev 
 * A is changed, if v != NULL v is computed 
 */

static int intdgeev(NspMatrix *A,NspMatrix **d,NspMatrix **v); 
static int intzgeev(NspMatrix *A,NspMatrix **d,NspMatrix **v);

int nsp_spec(NspMatrix *A, NspMatrix **d,NspMatrix **v) 
{
  /*  A = [] return empty matrices */ 
  int m = A->m,n=A->n;
  if ( A->mn == 0 ) 
    {
      if (( *d =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
      if ( v != NULL) 
	{
	  if (( *v =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;
	}
      return OK ; 
    }
  
  if (m != n) 
    { 
      Scierror("Error: first argument for spec should be square and it is (%dx%d)\n", 
	       m,n);
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

  /* checks that  A != Nan et != Inf */
  for ( i= 0 ; i < A->mn ; i++ ) 
    if (isinf (A->R[i]) || isnan (A->R[i]))
      {
	Scierror("Error: nan or inf in spec first argument\n"); 
	return FAIL;
      }

  wr = nsp_alloc_work_doubles(n);
  wi = nsp_alloc_work_doubles(n);
  if ( wr == NULL || wi == NULL ) goto err;

  if ( V != NULL )
    if ( (vr=nsp_matrix_create(NVOID,'r',n,n)) == NULLMAT ) goto err;

  lwork = -1;
  C2F(dgeev)("N",(V==NULL)?"N":"V", &n, A->R, &n, wr, wi, NULL, &n, (V==NULL)?NULL:vr->R, 
	     &n, qwork, &lwork, &info, 1L, 1L);
  lwork = (int) qwork[0];
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

  /* checks that  A != Nan et != Inf */
  for ( i= 0 ; i < A->mn ; i++ ) 
    if (nsp_isinf_c (&A->C[i]) || nsp_isnan_c (&A->C[i]))
      {
	Scierror("Error: nan or inf in spec first argument\n"); 
	return FAIL;
      }

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


/* OK
 * nsp_spec_sym 
 * spec(A) for symmetric matrices dgeev 
 * if flag == 'V',  V is returned in A 
 */
static int intdsyev(NspMatrix *A,NspMatrix **d,char flag);
static int intzheev(NspMatrix *A,NspMatrix **d,char flag);

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

  if ( A->rc_type == 'r' ) 
    return  intdsyev(A,d,flag); 
  else 
    return  intzheev(A,d,flag); 
}

static int intdsyev(NspMatrix *A,NspMatrix **d,char flag)
{
  int n=A->n;
  int info, lwork,i;
  NspMatrix *wr;
  double *dwork;
  double qwork[1];

  /* checks that  A != Nan et != Inf */
  for ( i= 0 ; i < A->mn ; i++ ) 
    if (isinf (A->R[i]) || isnan (A->R[i]))
      {
	Scierror("Error: nan or inf in spec first argument\n"); 
	return FAIL;
      }

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

static int intzheev(NspMatrix *A,NspMatrix **d,char flag)
{
  int n=A->n;
  int info, lwork,i;
  NspMatrix *wr=NULLMAT;
  double *rwork=NULL;
  doubleC *cwork=NULL, qwork[1];

  /* checks that  A != Nan et != Inf */
  for ( i= 0 ; i < A->mn ; i++ ) 
    if (nsp_isinf_c (&A->C[i]) || nsp_isnan_c (&A->C[i]))
      {
	Scierror("Error: nan or inf in spec first argument\n"); 
	return FAIL;
      }

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

/* OK 
 * nsp_inv (triangular case added)
 */
static int intdgetri(NspMatrix *A);
static int intzgetri(NspMatrix *A);

int nsp_inv(NspMatrix *A) 
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
	return intdgetri(A);
      else
	return intzgetri(A);
    }
  else  /* triangular case */
    {
      if ( A->rc_type == 'r')
	C2F(dtrtri) (&tflag, "N", &n, A->R, &n, &info, 1L, 1L);
      else
	C2F(ztrtri) (&tflag, "N", &n, A->C, &n, &info, 1L, 1L);

      if (info == 0) 
	return OK; 
      else 
	{
	  Scierror("inv: matrix is singular\n");
	  return FAIL;
	}
    }
}

static int intdgetri(NspMatrix *A)
{
  int *ipiv=NULL;
  double *dwork=NULL, qwork[1];
  int info, lwork, n=A->n ;

  if ( (ipiv = nsp_alloc_int(n)) == NULL ) goto err;

  C2F(dgetrf)(&n, &n, A->R, &n, ipiv, &info);
  if (info != 0) 
    {
      Scierror("inv: matrix is singular\n");
      goto err;
    };

  lwork = -1;
  C2F(dgetri)(&n, A->R, &n, ipiv, qwork, &lwork, &info);
  lwork = (int) qwork[0];
  if ( (dwork = nsp_alloc_doubles(lwork)) == NULL ) goto err;
  C2F(dgetri)(&n, A->R, &n, ipiv, dwork, &lwork, &info);

  FREE(dwork); FREE(ipiv);
  return OK;

 err:
  FREE(dwork); FREE(ipiv);
  return FAIL;
} 

static int intzgetri(NspMatrix *A)
{
  int *ipiv=NULL;
  doubleC *cwork=NULL, qwork[1];
  int info, lwork, n = A->n;

  if ( (ipiv = nsp_alloc_int(n)) == NULL ) goto err;

  C2F(zgetrf)(&n, &n, A->C, &n, ipiv, &info);
  if (info != 0) 
    {
      Scierror("inv: matrix is singular\n");
      goto err;
    };

  lwork = -1;
  C2F(zgetri)(&n, A->C, &n, ipiv, qwork, &lwork, &info);
  lwork = (int) qwork[0].r;
  if ( (cwork = nsp_alloc_doubleC(lwork)) == NULL ) goto err;
  C2F(zgetri)(&n, A->C, &n, ipiv, cwork, &lwork, &info);

  FREE(cwork); FREE(ipiv);
  return OK;

 err:
  FREE(cwork); FREE(ipiv);
  return FAIL;
} 


/* OK
 * nsp_rcond(A)
 */

static int intdgecon(NspMatrix *A,double *rcond);
static int intzgecon(NspMatrix *A,double *rcond);
static int inttrcon(NspMatrix *A, char tri_type, double *rcond);

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
  else 
    {
      Scierror("Error: something wrong in dgetrf\n"); 
      /* FIXME : the only pb is a 0 pivot (so we could improve the message) */
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
  else 
    {
      Scierror("Error: something wrong in zgetrf\n"); 
      /* FIXME : the only pb is a null pivot (so we could improve the message) */
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


/* OK 
 * nsp_cholesky
 * A is changed 
 */

int nsp_cholesky(NspMatrix *A) 
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
	Scierror("Error: matrix is not positive definite\n"); 
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
 * XXX  Attention rwork est pas créée a finir 
 */


int intzgees0(NspMatrix *A,NspMatrix **U,int (*F)(doubleC *w), NspMatrix **Sdim) 
{
  NspMatrix *W,*dwork,*rwork=NULLMAT,*iwork=NULLMAT;
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
      sort = "S";
    }

  if ( U != NULL ) 
    {
      if (( *U =nsp_matrix_create(NVOID,A->rc_type,m,n)) == NULLMAT) return FAIL;   
      C2F(zgees)("V",sort, F, &n,A->C,&n, &sdim,W->C,(*U)->C,
		 &n, dwork->C, &lworkMin,rwork->R, iwork->I, &info, 4L, 4L);
    }
  else 
    { 
      C2F(zgees)("N",sort, F, &n,A->C,&n, &sdim,W->C,NULL,
		 &n, dwork->C, &lworkMin,rwork->R, iwork->I, &info, 4L, 4L);
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
  double  *Vsl=NULL,*Vsr=NULL;
  NspMatrix *alphar,*alphai,*beta,*dwork,*iwork=NULLMAT;
  int info,lworkMin,sdim;
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
	     dwork->R, &lworkMin, iwork->I, &info, 1L, 1L, 1L);
  
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
  doubleC  *Vsl=NULL,*Vsr=NULL;
  NspMatrix *alpha,*beta,*dwork,*iwork=NULLMAT,*rwork;
  int info,lworkMin,sdim;
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
	     dwork->C, &lworkMin,rwork->R,iwork->I, &info, 1L, 1L, 1L);
  
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
 * [al,be,Q,Z] = gspec(A,B) returns in addition the matrix Q and Z
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


/* nsp_vector_norm 
 *   norm(A,p) 
 */ 
static double intzvnorm(NspMatrix *A, double p);
static double intdvnorm(NspMatrix *A, double p);

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
  double norm=0.0, scale, xi_abs, temp;

  if ( p == 1.0 )
    for ( i = 0 ; i < A->mn ; i++ )
      norm += fabs(A->R[i]);
  else
    {
      scale = fabs(A->R[0]);
      for ( i = 1 ; i < A->mn ; i++ )
	{
	  xi_abs = fabs(A->R[i]);
	  if ( xi_abs > scale ) scale = xi_abs;
	}
      if ( isinf(p) )
	norm = scale;
      else if ( p == 2.0 )
	{
	  for ( i = 0 ; i < A->mn ; i++ )
	    {
	      temp = fabs(A->R[i])/scale;
	      norm += temp*temp;
	    }
	  norm = scale*sqrt(norm);
	}
      else
	{
	  for ( i = 0 ; i < A->mn ; i++ )
	    norm += pow(fabs(A->R[i])/scale, p);
	  norm = scale*pow(norm,1.0/p);
	}
    }
  return norm;
}

static double intzvnorm(NspMatrix *A, double p)
{
  int i;
  double norm=0.0, scale=0.0, temp, *work;

  if ( p == 1.0 )
    {
      for ( i = 0 ; i < A->mn ; i++ )
	norm +=  nsp_abs_c(&(A->C[i]));
      return norm;
    }
  
  if ( (work=nsp_alloc_work_doubles(A->mn)) == NULL ) return -1.0;

  for ( i = 0 ; i < A->mn ; i++ )
    {
      work[i] = nsp_abs_c(&(A->C[i]));
      if ( work[i] > scale ) scale = work[i];
    }

  if ( isinf(p) )
    norm = scale;
  else if ( p == 2.0 )
    {
      for ( i = 0 ; i < A->mn ; i++ )
	{
	  temp = work[i]/scale;
	  norm += temp*temp;
	}
      norm = scale*sqrt(norm);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++ )
	norm += pow(work[i]/scale, p);
      norm = scale*pow(norm,1.0/p);
    }

  FREE(work);
  return norm;
}

  
/* nsp_matrix_norm 
 *   norm(A,'xxx') 
 *   'M' '1' 'I' 'F' 
 */ 

static double intznorm(NspMatrix *A,char flag);
static double intdnorm(NspMatrix *A,char flag);

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
 * @A: a #NspMatrix 
 * 
 * computes the exponential of the matrix @A.
 * caution @A is overwritten with the result.
 * 
 * Return value: FAIl or OK
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


/*
 *   Res=nsp_mat_bdiv_lsq(A,B) A \ B. Contributed by Bruno Pincon
 */

int nsp_mat_bdiv_lsq(NspMatrix *A, NspMatrix *B)
{  
  int mA = A->m, nA = A->n, mB = B->m, nB = B->n; /* mA must be equal to mB */
  int mx = A->n, nx = B->n, ldB, ix, iB, j;
  int *jpvt = NULL, info, lwork, rank;
  double qrwork[1], *rwork = NULL;
  doubleC qcwork[1], *cwork = NULL;
  double rcond = DBL_EPSILON;  /* FIXME to be choosen more correctly */

  /* FIXME: - to merge with nsp_mat_lsq
   *        - when B is complex while A is real, something
   *          better than complexify A can be done
   */
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

  if ( mx > mB )  
    /* enlarge B so that it can contains the solution x (needed by lapack) */
    {
      iB = B->mn-1;
      if ((nsp_matrix_resize(B,mx,nx)) ==  FAIL) return FAIL;
      if ( B->rc_type == 'r' )
	for ( j = nB-1 ; j > 0 ; j--)   /* the first column is already good (so j>0) */
	  for ( ix = j*mx+mB-1 ; ix >= j*mx ; ix--, iB-- )
	    B->R[ix] = B->R[iB];
      else
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
       
  if ( A->rc_type == 'r' )
    { 
      lwork = -1;  /* query work size */
      C2F(dgelsy)(&mA, &nA, &nB, A->R, &mA, B->R, &ldB, jpvt, &rcond, &rank, qrwork, &lwork, &info);
      lwork = (int) qrwork[0];
      if ( (rwork = nsp_alloc_work_doubles(lwork)) == NULL ) goto err;
      C2F(dgelsy)(&mA, &nA, &nB, A->R, &mA, B->R, &ldB, jpvt, &rcond, &rank, rwork, &lwork, &info);
    }
  else 
    {
      lwork = -1;  /* query work size */
      C2F(zgelsy)(&mA, &nA, &nB, A->C, &mA, B->C, &ldB, jpvt, &rcond, &rank, qcwork, &lwork, rwork, &info);
      lwork = (int) qcwork[0].r;
      rwork = nsp_alloc_work_doubles(2*nA); 
      cwork = nsp_alloc_work_doubleC(lwork);
      if ( (rwork == NULL) || (cwork==NULL) ) goto err;
      C2F(zgelsy)(&mA, &nA, &nB, A->C, &mA, B->C, &ldB, jpvt, &rcond, &rank, cwork, &lwork, rwork, &info);
    }
  
  if ( mx < mB )   /* free a part of B which is not needed */
    {
      /* first compress B (the solution x is in B but with a "leading" 
       * dimension mB  => transform with a "leading" dimension mx ...) 
       */
      ix = mx;
      if ( B->rc_type == 'r' )
	for ( j = 1 ; j < nB ; j++)   /* the first column is already good */
	  for ( iB = j*mB ; iB < j*mB+mx ; iB++, ix++)
	    B->R[ix] = B->R[iB];
      else
	for ( j = 1 ; j < nB ; j++)   /* the first column is already good */
	  for ( iB = j*mB ; iB < j*mB+mx ; iB++, ix++)
	    B->C[ix] = B->C[iB];
      /* now we can free the part of B which is not used anymore */
      if (  nsp_matrix_resize(B, mx, nx) == FAIL )
	goto err;
    }

  FREE(jpvt); FREE(rwork); FREE(cwork);
  if ( rank < Min(mA,nA) )
    Sciprintf("\n warning: matrix is rank-deficient m=%d, n=%d, rank=%d \n", mA, nA, rank);

  return OK;

 err:
  FREE(jpvt); FREE(rwork); FREE(cwork);
  return FAIL;
      
}

/* mA must be equal to nA */
int nsp_mat_bdiv_square(NspMatrix *A, NspMatrix *B, double *rcond)
{  
  int n=A->m, nrhs=B->n; 
  int *ipiv=NULL, *iwork=NULL, info;
  double anorm, *rwork=NULL;
  doubleC *cwork=NULL;

  /* FIXME: - when B is complex while A is real, something
   *          better than complexify A can be done
   */
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
      anorm = C2F(dlange) ("1", &n, &n, A->R, &n, rwork, 1); /* rwork is used only for inf norm */
      C2F(dgetrf) (&n, &n, A->R, &n, ipiv, &info);
      if ( info != 0 )  /* a pivot is exactly zero */
	{
	  *rcond = 0.0; FREE(ipiv); return OK;
	}
      if ( (iwork = nsp_alloc_work_int(n)) == NULL ) goto err;
      if ( (rwork = nsp_alloc_work_doubles(4*n)) == NULL ) goto err;      

      C2F(dgecon) ("1", &n, A->R, &n, &anorm, rcond, rwork, iwork, &info, 1);
      if ( *rcond <=  DBL_EPSILON ) /* matrix is too badly conditionned */
	{
	  FREE(rwork); FREE(iwork); FREE(ipiv); return OK;
	}
      C2F(dgetrs) ("N", &n, &nrhs, A->R, &n, ipiv, B->R, &n, &info, 1);
      FREE(rwork); FREE(iwork); FREE(ipiv); return OK;
    }
  else
    {
      anorm = C2F(zlange) ("1", &n, &n, A->C, &n, rwork, 1); /* rwork is used only for inf norm */
      C2F(zgetrf) (&n, &n, A->C, &n, ipiv, &info);
      if ( info != 0 )  /* a pivot is exactly zero */
	{
	  *rcond = 0.0; FREE(ipiv); return OK;
	}
      if ( (rwork = nsp_alloc_doubles(2*n)) == NULL ) goto err;      
      if ( (cwork = nsp_alloc_doubleC(2*n)) == NULL ) goto err;

      C2F(zgecon) ("1", &n, A->C, &n, &anorm, rcond, cwork, rwork, &info, 1);
      if ( *rcond <=  DBL_EPSILON )  /* matrix is too badly conditionned */
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
 * @info: an int 0 if all is OK else a zero pivot have been met.
 * 
 * solve a linear triangular system A X = B, B is overwritten by the solution X
 *
 * Return value: OK or FAIL (due to  malloc failure)
 **/
int nsp_mat_bdiv_triangular(NspMatrix *A, NspMatrix *B, char tri_type, int *info)
{  
  int m = A->m, mn, i;
  double *temp;

  if ( A->rc_type == 'c' ) 
    if ( B->rc_type == 'r' ) 
      if (nsp_mat_set_ival(B,0.00) == FAIL ) return FAIL;

  if ( A->rc_type == 'r' )
    {
      if ( B->rc_type == 'r' )
	C2F(dtrtrs) (&tri_type, "N", "N", &m, &(B->n), A->R, &m, B->R, &m, info, 1,1,1);
      else  /* B is complex */
	{
	  mn = m * B->n;
	  if ( (temp = nsp_alloc_work_doubles(mn)) == NULL ) return FAIL;
	  for ( i = 0 ; i < mn ; i++ ) temp[i] = B->C[i].r;
	  C2F(dtrtrs) (&tri_type, "N", "N", &m, &(B->n), A->R, &m, temp, &m, info, 1,1,1);
	  if ( *info != 0 ) { FREE(temp); return FAIL;}
	  for ( i = 0 ; i < mn ; i++ )  { B->C[i].r = temp[i]; temp[i] = B->C[i].i;}
	  C2F(dtrtrs) (&tri_type, "N", "N", &m, &(B->n), A->R, &m, temp, &m, info, 1,1,1);
	  for ( i = 0 ; i < mn ; i++ )  B->C[i].i = temp[i];
	  FREE(temp);
	}
    }
  else
    C2F(ztrtrs) (&tri_type, "N", "N", &m, &(B->n), A->C, &m, B->C, &m, info, 1,1,1);

  return OK;
}

