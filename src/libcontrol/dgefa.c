/* dgefa.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
/*
 *    dgefa factors a double precision matrix by gaussian elimination. 
 * 
 *    dgefa is usually called by dgeco, but it can be called 
 *    directly with a saving in time if  rcond  is not needed. 
 *    (time for dgeco) = (1 + 9/n)*(time for dgefa) . 
 * 
 *     subroutine dgefa(a,lda,n,ipvt,info) 
 *    on entry 
 *       a       double precision(lda, n) 
 *               the matrix to be factored. 
 *       lda     int 
 *               the leading dimension of the array  a . 
 *       n       int 
 *               the order of the matrix  a . 
 *    on return 
 *       a       an upper triangular matrix and the multipliers 
 *               which were used to obtain it. 
 *               the factorization can be written  a = l*u  where 
 *               l  is a product of permutation and unit lower 
 *               triangular matrices and  u  is upper triangular. 
 *       ipvt    int(n) 
 *               an int vector of pivot indices. 
 *       info    int 
 *               = 0  normal value. 
 *               = k  if  u(k,k) .eq. 0.0 .  this is not an error 
 *                    condition for this subroutine, but it does 
 *                    indicate that dgesl or dgedi will divide by zero 
 *                    if called.  use  rcond  in dgeco for a reliable 
 *                    indication of singularity. 
 *    linpack. this version dated 08/14/78 . 
 *    cleve moler, university of new mexico, argonne national lab. 
 */

/* Table of constant values */

static int c__1 = 1;

int nsp_ctrlpack_dgefa (double *a, int *lda, int *n, int *ipvt, int *info)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2, i__3;

  /* Local variables */
  int j, k, l;
  double t;
  int kp1, nm1;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;

  /* Function Body */
  *info = 0;
  nm1 = *n - 1;
  if (nm1 < 1)
    {
      goto L70;
    }
  i__1 = nm1;
  for (k = 1; k <= i__1; ++k)
    {
      kp1 = k + 1;
      /* 
       *       find l = pivot index 
       * 
       */
      i__2 = *n - k + 1;
      l = C2F (idamax) (&i__2, &a[k + k * a_dim1], &c__1) + k - 1;
      ipvt[k] = l;
      /* 
       *       zero pivot implies this column already triangularized 
       * 
       */
      if (a[l + k * a_dim1] == 0.)
	{
	  goto L40;
	}
      /* 
       *          interchange if necessary 
       * 
       */
      if (l == k)
	{
	  goto L10;
	}
      t = a[l + k * a_dim1];
      a[l + k * a_dim1] = a[k + k * a_dim1];
      a[k + k * a_dim1] = t;
    L10:
      /* 
       *          compute multipliers 
       * 
       */
      t = -1. / a[k + k * a_dim1];
      i__2 = *n - k;
      C2F (dscal) (&i__2, &t, &a[k + 1 + k * a_dim1], &c__1);
      /* 
       *          row elimination with column indexing 
       * 
       */
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  t = a[l + j * a_dim1];
	  if (l == k)
	    {
	      goto L20;
	    }
	  a[l + j * a_dim1] = a[k + j * a_dim1];
	  a[k + j * a_dim1] = t;
	L20:
	  i__3 = *n - k;
	  C2F (daxpy) (&i__3, &t, &a[k + 1 + k * a_dim1], &c__1,
		       &a[k + 1 + j * a_dim1], &c__1);
	  /* L30: */
	}
      goto L50;
    L40:
      *info = k;
    L50:
      /* L60: */
      ;
    }
 L70:
  ipvt[*n] = *n;
  if (a[*n + *n * a_dim1] == 0.)
    {
      *info = *n;
    }
  return 0;
}				/* dgefa_ */
