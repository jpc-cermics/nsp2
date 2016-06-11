/* wgedi.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static double c_b12 = 1.;
static double c_b13 = 0.;
static int c__1 = 1;

int
nsp_ctrlpack_wgedi (double *ar, double *ai, int *lda, int *n, int *ipvt,
		    double *detr, double *deti, double *workr, double *worki,
		    int *job)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2;

  /* Local variables */
  int i__, j, k, l;
  int kb;
  double ti, tr;
  int kp1, nm1;
  double ten;

  /*    Copyright INRIA 
   *!purpose 
   * 
   *    wgedi computes the determinant and inverse of a matrix 
   *    using the factors computed by wgeco or wgefa. 
   * 
   *!calling sequence 
   * 
   *     subroutine wgedi(ar,ai,lda,n,ipvt,detr,deti,workr,worki,job) 
   *    on entry 
   * 
   *       a       double-complex(lda, n) 
   *               the output from wgeco or wgefa. 
   * 
   *       lda     int 
   *               the leading dimension of the array  a . 
   * 
   *       n       int 
   *               the order of the matrix  a . 
   * 
   *       ipvt    int(n) 
   *               the pivot vector from wgeco or wgefa. 
   * 
   *       work    double-complex(n) 
   *               work vector.  contents destroyed. 
   * 
   *       job     int 
   *               = 11   both determinant and inverse. 
   *               = 01   inverse only. 
   *               = 10   determinant only. 
   * 
   *    on return 
   * 
   *       a       inverse of original matrix if requested. 
   *               otherwise unchanged. 
   * 
   *       det     double-complex(2) 
   *               determinant of original matrix if requested. 
   *               otherwise not referenced. 
   *               determinant = det(1) * 10.0**det(2) 
   *               with  1.0 .le. cabs1(det(1) .lt. 10.0 
   *               or  det(1) .eq. 0.0 . 
   * 
   *    error condition 
   * 
   *       a division by zero will occur if the input factor contains 
   *       a zero on the diagonal and the inverse is requested. 
   *       it will not occur if the subroutines are called correctly 
   *       and if wgeco has set rcond .gt. 0.0 or wgefa has set 
   *       info .eq. 0 . 
   * 
   *!originator 
   *    linpack. this version dated 07/01/79 . 
   *    cleve moler, university of new mexico, argonne national lab. 
   * 
   *!auxiliary routines 
   * 
   *    blas waxpy,wscal,wswap 
   *    fortran abs,mod 
   * 
   *! 
   *    internal variables 
   * 
   * 
   * 
   *    compute determinant 
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
  --detr;
  --deti;
  --workr;
  --worki;

  /* Function Body */
  if (*job / 10 == 0)
    {
      goto L80;
    }
  detr[1] = 1.;
  deti[1] = 0.;
  detr[2] = 0.;
  deti[2] = 0.;
  ten = 10.;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (ipvt[i__] == i__)
	{
	  goto L10;
	}
      detr[1] = -detr[1];
      deti[1] = -deti[1];
    L10:
      nsp_calpack_wmul (&ar[i__ + i__ * ar_dim1], &ai[i__ + i__ * ai_dim1],
			&detr[1], &deti[1], &detr[1], &deti[1]);
      /*          ...exit 
       *       ...exit 
       */
      if (Abs (detr[1]) + Abs (deti[1]) == 0.)
	{
	  goto L70;
	}
    L20:
      if (Abs (detr[1]) + Abs (deti[1]) >= 1.)
	{
	  goto L30;
	}
      detr[1] = ten * detr[1];
      deti[1] = ten * deti[1];
      detr[2] += -1.;
      deti[2] += 0.;
      goto L20;
    L30:
    L40:
      if (Abs (detr[1]) + Abs (deti[1]) < ten)
	{
	  goto L50;
	}
      detr[1] /= ten;
      deti[1] /= ten;
      detr[2] += 1.;
      deti[2] += 0.;
      goto L40;
    L50:
      /* L60: */
      ;
    }
 L70:
 L80:
  /* 
   *    compute inverse(u) 
   * 
   */
  if (*job % 10 == 0)
    {
      goto L160;
    }
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      nsp_calpack_wdiv (&c_b12, &c_b13, &ar[k + k * ar_dim1],
			&ai[k + k * ai_dim1], &ar[k + k * ar_dim1],
			&ai[k + k * ai_dim1]);
      tr = -ar[k + k * ar_dim1];
      ti = -ai[k + k * ai_dim1];
      i__2 = k - 1;
      nsp_calpack_wscal (&i__2, &tr, &ti, &ar[k * ar_dim1 + 1],
			 &ai[k * ai_dim1 + 1], &c__1);
      kp1 = k + 1;
      if (*n < kp1)
	{
	  goto L100;
	}
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  tr = ar[k + j * ar_dim1];
	  ti = ai[k + j * ai_dim1];
	  ar[k + j * ar_dim1] = 0.;
	  ai[k + j * ai_dim1] = 0.;
	  nsp_calpack_waxpy (&k, &tr, &ti, &ar[k * ar_dim1 + 1],
			     &ai[k * ai_dim1 + 1], &c__1,
			     &ar[j * ar_dim1 + 1], &ai[j * ai_dim1 + 1],
			     &c__1);
	  /* L90: */
	}
    L100:
      /* L110: */
      ;
    }
  /* 
   *       form inverse(u)*inverse(l) 
   * 
   */
  nm1 = *n - 1;
  if (nm1 < 1)
    {
      goto L150;
    }
  i__1 = nm1;
  for (kb = 1; kb <= i__1; ++kb)
    {
      k = *n - kb;
      kp1 = k + 1;
      i__2 = *n;
      for (i__ = kp1; i__ <= i__2; ++i__)
	{
	  workr[i__] = ar[i__ + k * ar_dim1];
	  worki[i__] = ai[i__ + k * ai_dim1];
	  ar[i__ + k * ar_dim1] = 0.;
	  ai[i__ + k * ai_dim1] = 0.;
	  /* L120: */
	}
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  tr = workr[j];
	  ti = worki[j];
	  nsp_calpack_waxpy (n, &tr, &ti, &ar[j * ar_dim1 + 1],
			     &ai[j * ai_dim1 + 1], &c__1,
			     &ar[k * ar_dim1 + 1], &ai[k * ai_dim1 + 1],
			     &c__1);
	  /* L130: */
	}
      l = ipvt[k];
      if (l != k)
	{
	  nsp_calpack_wswap (n, &ar[k * ar_dim1 + 1], &ai[k * ai_dim1 + 1],
			     &c__1, &ar[l * ar_dim1 + 1],
			     &ai[l * ai_dim1 + 1], &c__1);
	}
      /* L140: */
    }
 L150:
 L160:
  return 0;
}				/* wgedi_ */
