/* dgedi.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 *    dgedi computes the determinant and inverse of a matrix 
 *    using the factors computed by dgeco or dgefa. 
 * 
 *     subroutine dgedi(a,lda,n,ipvt,det,work,job) 
 *    on entry 
 *       a       double precision(lda, n) 
 *               the output from dgeco or dgefa. 
 *       lda     int 
 *               the leading dimension of the array  a . 
 *       n       int 
 *               the order of the matrix  a . 
 *       ipvt    int(n) 
 *               the pivot vector from dgeco or dgefa. 
 *       work    double precision(n) 
 *               work vector.  contents destroyed. 
 *       job     int 
 *               = 11   both determinant and inverse. 
 *               = 01   inverse only. 
 *               = 10   determinant only. 
 *    on return 
 *       a       inverse of original matrix if requested. 
 *               otherwise unchanged. 
 *       det     double precision(2) 
 *               determinant of original matrix if requested. 
 *               otherwise not referenced. 
 *               determinant = det(1) * 10.0**det(2) 
 *               with  1.0 .le. Abs(det(1)) .lt. 10.0 
 *               or  det(1) .eq. 0.0 . 
 * 
 *    error condition 
 * 
 *       a division by zero will occur if the input factor contains 
 *       a zero on the diagonal and the inverse is requested. 
 *       it will not occur if the subroutines are called correctly 
 *       and if dgeco has set rcond .gt. 0.0 or dgefa has set 
 *       info .eq. 0 . 
 *
 *    linpack. this version dated 08/14/78 . 
 *    cleve moler, university of new mexico, argonne national lab. 
 */

static int c__1 = 1;

int
nsp_ctrlpack_dgedi (double *a, int *lda, int *n, int *ipvt, double *det,
		    double *work, int *job)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;

  /* Local variables */
  int i__, j, k, l;
  double t;
  int kb, kp1, nm1;
  double ten;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;
  --det;
  --work;

  /* Function Body */
  if (*job / 10 == 0)
    {
      goto L70;
    }
  det[1] = 1.;
  det[2] = 0.;
  ten = 10.;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (ipvt[i__] != i__)
	{
	  det[1] = -det[1];
	}
      det[1] = a[i__ + i__ * a_dim1] * det[1];
      /*       ...exit 
       */
      if (det[1] == 0.)
	{
	  goto L60;
	}
    L10:
      if (Abs (det[1]) >= 1.)
	{
	  goto L20;
	}
      det[1] = ten * det[1];
      det[2] += -1.;
      goto L10;
    L20:
    L30:
      if (Abs (det[1]) < ten)
	{
	  goto L40;
	}
      det[1] /= ten;
      det[2] += 1.;
      goto L30;
    L40:
      /* L50: */
      ;
    }
 L60:
 L70:
  /* 
   *    compute inverse(u) 
   * 
   */
  if (*job % 10 == 0)
    {
      goto L150;
    }
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      a[k + k * a_dim1] = 1. / a[k + k * a_dim1];
      t = -a[k + k * a_dim1];
      i__2 = k - 1;
      C2F (dscal) (&i__2, &t, &a[k * a_dim1 + 1], &c__1);
      kp1 = k + 1;
      if (*n < kp1)
	{
	  goto L90;
	}
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  t = a[k + j * a_dim1];
	  a[k + j * a_dim1] = 0.;
	  C2F (daxpy) (&k, &t, &a[k * a_dim1 + 1], &c__1,
		       &a[j * a_dim1 + 1], &c__1);
	  /* L80: */
	}
    L90:
      /* L100: */
      ;
    }
  /* 
   *       form inverse(u)*inverse(l) 
   * 
   */
  nm1 = *n - 1;
  if (nm1 < 1)
    {
      goto L140;
    }
  i__1 = nm1;
  for (kb = 1; kb <= i__1; ++kb)
    {
      k = *n - kb;
      kp1 = k + 1;
      i__2 = *n;
      for (i__ = kp1; i__ <= i__2; ++i__)
	{
	  work[i__] = a[i__ + k * a_dim1];
	  a[i__ + k * a_dim1] = 0.;
	  /* L110: */
	}
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  t = work[j];
	  C2F (daxpy) (n, &t, &a[j * a_dim1 + 1], &c__1,
		       &a[k * a_dim1 + 1], &c__1);
	  /* L120: */
	}
      l = ipvt[k];
      if (l != k)
	{
	  C2F (dswap) (n, &a[k * a_dim1 + 1], &c__1,
		       &a[l * a_dim1 + 1], &c__1);
	}
      /* L130: */
    }
 L140:
 L150:
  return 0;
}				/* dgedi_ */
