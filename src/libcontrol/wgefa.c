/* wgefa.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static int c__1 = 1;
static double c_b7 = -1.;
static double c_b8 = 0.;

int
nsp_ctrlpack_wgefa (double *ar, double *ai, int *lda, int *n, int *ipvt,
		    int *info)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Local variables */
  int j, k, l;
  double ti, tr;
  int kp1, nm1;

  /*    Copyright INRIA 
   *!purpose 
   * 
   *    wgefa factors a double-complex matrix by gaussian elimination. 
   * 
   *    wgefa is usually called by wgeco, but it can be called 
   *    directly with a saving in time if  rcond  is not needed. 
   *    (time for wgeco) = (1 + 9/n)*(time for wgefa) . 
   * 
   *!calling sequence 
   * 
   *     subroutine wgefa(ar,ai,lda,n,ipvt,info) 
   *    on entry 
   * 
   *       a       double-complex(lda, n) 
   *               the matrix to be factored. 
   * 
   *       lda     int 
   *               the leading dimension of the array  a . 
   * 
   *       n       int 
   *               the order of the matrix  a . 
   * 
   *    on return 
   * 
   *       a       an upper triangular matrix and the multipliers 
   *               which were used to obtain it. 
   *               the factorization can be written  a = l*u  where 
   *               l  is a product of permutation and unit lower 
   *               triangular matrices and  u  is upper triangular. 
   * 
   *       ipvt    int(n) 
   *               an int vector of pivot indices. 
   * 
   *       info    int 
   *               = 0  normal value. 
   *               = k  if  u(k,k) .eq. 0.0 .  this is not an error 
   *                    condition for this subroutine, but it does 
   *                    indicate that wgesl or wgedi will divide by zero 
   *                    if called.  use  rcond  in wgeco for a reliable 
   *                    indication of singularity. 
   * 
   *!originator 
   *    linpack. this version dated 07/01/79 . 
   *    cleve moler, university of new mexico, argonne national lab. 
   * 
   *!auxiliary routines 
   * 
   *    blas waxpy,wscal,iwamax 
   *    fortran abs 
   * 
   *! 
   *    internal variables 
   * 
   * 
   * 
   *    gaussian elimination with partial pivoting 
   * 
   */
  /* Parameter adjustments */
  ai_dim1 = *lda;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *lda;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
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
      l =
	nsp_calpack_iwamax (&i__2, &ar[k + k * ar_dim1],
			    &ai[k + k * ai_dim1], &c__1) + k - 1;
      ipvt[k] = l;
      /* 
       *       zero pivot implies this column already triangularized 
       * 
       */
      if ((d__1 = ar[l + k * ar_dim1], Abs (d__1)) + (d__2 =
						      ai[l + k * ai_dim1],
						      Abs (d__2)) == 0.)
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
      tr = ar[l + k * ar_dim1];
      ti = ai[l + k * ai_dim1];
      ar[l + k * ar_dim1] = ar[k + k * ar_dim1];
      ai[l + k * ai_dim1] = ai[k + k * ai_dim1];
      ar[k + k * ar_dim1] = tr;
      ai[k + k * ai_dim1] = ti;
    L10:
      /* 
       *          compute multipliers 
       * 
       */
      nsp_calpack_wdiv (&c_b7, &c_b8, &ar[k + k * ar_dim1],
			&ai[k + k * ai_dim1], &tr, &ti);
      i__2 = *n - k;
      nsp_calpack_wscal (&i__2, &tr, &ti, &ar[k + 1 + k * ar_dim1],
			 &ai[k + 1 + k * ai_dim1], &c__1);
      /* 
       *          row elimination with column indexing 
       * 
       */
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  tr = ar[l + j * ar_dim1];
	  ti = ai[l + j * ai_dim1];
	  if (l == k)
	    {
	      goto L20;
	    }
	  ar[l + j * ar_dim1] = ar[k + j * ar_dim1];
	  ai[l + j * ai_dim1] = ai[k + j * ai_dim1];
	  ar[k + j * ar_dim1] = tr;
	  ai[k + j * ai_dim1] = ti;
	L20:
	  i__3 = *n - k;
	  nsp_calpack_waxpy (&i__3, &tr, &ti, &ar[k + 1 + k * ar_dim1],
			     &ai[k + 1 + k * ai_dim1], &c__1,
			     &ar[k + 1 + j * ar_dim1],
			     &ai[k + 1 + j * ai_dim1], &c__1);
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
  if ((d__1 = ar[*n + *n * ar_dim1], Abs (d__1)) + (d__2 =
						    ai[*n + *n * ai_dim1],
						    Abs (d__2)) == 0.)
    {
      *info = *n;
    }
  return 0;
}				/* wgefa_ */
