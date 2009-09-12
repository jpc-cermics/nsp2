#include "integ.h"

/*!purpose 
 * 
 *    dgefa factors a double precision matrix by gaussian elimination. 
 * 
 *    dgefa is usually called by dgeco, but it can be called 
 *    directly with a saving in time if  rcond  is not needed. 
 *    (time for dgeco) = (1 + 9/n)*(time for dgefa) . 
 * 
 *!calling sequence 
 * 
 *     subroutine dgefa(a,lda,n,ipvt,info) 
 *    on entry 
 * 
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
 *!originator 
 *    linpack. this version dated 08/14/78 . 
 *    cleve moler, university of new mexico, argonne national lab. 
 * 
 *!auxiliary routines 
 * 
 *    blas daxpy,dscal,idamax 
 * 
 *! 
 *    internal variables 
 *    gaussian elimination with partial pivoting 
 */

int C2F(dgefa) (double *a, int *lda, int *n, int *ipvt, int *info)
{
  int c__1 = 1;
  int a_dim1, a_offset, i1, i2, i3;

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
  i1 = nm1;
  for (k = 1; k <= i1; ++k)
    {
      kp1 = k + 1;
      /* 
       *       find l = pivot index 
       * 
       */
      i2 = *n - k + 1;
      l = C2F (idamax) (&i2, &a[k + k * a_dim1], &c__1) + k - 1;
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
      i2 = *n - k;
      C2F (dscal) (&i2, &t, &a[k + 1 + k * a_dim1], &c__1);
      /* 
       *          row elimination with column indexing 
       * 
       */
      i2 = *n;
      for (j = kp1; j <= i2; ++j)
	{
	  t = a[l + j * a_dim1];
	  if (l == k)
	    {
	      goto L20;
	    }
	  a[l + j * a_dim1] = a[k + j * a_dim1];
	  a[k + j * a_dim1] = t;
	L20:
	  i3 = *n - k;
	  C2F (daxpy) (&i3, &t, &a[k + 1 + k * a_dim1], &c__1,
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
}
