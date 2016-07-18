/* dexpm1.f -- translated by f2c (version 19961017).
 */

#include "ctrlpack.h"

/* 
 *     compute the exponential of a matrix a by the pade's 
 *     approximants(subroutine pade).a block diagonalization 
 *        is performed prior call pade. 
 *    subroutine dexpm1(ia,n,a,ea,iea,w,iw,ierr) 
 *
 *    int ia,n,iw,ierr 
 *    double precision a,ea,w 
 *    dimension a(ia,n),ea(iea,n),w(*),iw(*) 
 * 
 *     ia: the leading dimension of array a. 
 *     n: the order of the matrices a,ea,x,xi . 
 *     a: the real double precision array that contains the n*n matrix a 
 *     ea: the array that contains the n*n exponential of a. 
 *     iea : the leading dimension of array ea 
 *     w : work space array of size: n*(2*ia+2*n+5) 
 *     iw : int work space array of size 2*n 
 *     ierr: =0 if the prosessus is normal. 
 *              =-1 if n>ia. 
 *              =-2 if the block diagonalization is not possible. 
 *              =-4 if the subroutine dpade can not computes exp(a) 
 * 
 *    J. Roche laboratoire d'automatique de grenoble 
 */

#define dcoeff_1 pade_dcoeff

static double c_b6 = 0.;
static int c__1 = 1;

int
nsp_ctrlpack_dexpm1 (int *ia, int *n, double *a, double *ea, int *iea,
		     double *w, int *iw, int *ierr)
{
  static double zero = 0.;

  int a_dim1, a_offset, ea_dim1, ea_offset, i__1, i__2;
  double d__1, d__2;

  int fail;
  double bvec;
  int i__, j, k;
  double bbvec, alpha;
  int kscal;
  double anorm;
  int ni;
  double rn;
  int kw, kx, kei, nii, kbs, ker, kiw, kxi;


  a_dim1 = *ia;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  ea_dim1 = *iea;
  ea_offset = ea_dim1 + 1;
  ea -= ea_offset;
  --w;
  --iw;

  /* Function Body */
  dcoeff_1.n = -1;
  /* 
   */
  *ierr = 0;
  kscal = 1;
  kx = kscal + *n;
  kxi = kx + *n * *ia;
  ker = kxi + *n * *ia;
  kei = ker + *n;
  kw = kei + *n;
  /* 
   */
  kbs = 1;
  kiw = kbs + *n;
  /* 
   */
  if (*n > *ia)
    {
      goto L110;
    }
  /* 
   * balance the matrix a 
   * compute the norm one of a. 
   */
  anorm = 0.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      alpha = zero;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  alpha += (d__1 = a[i__ + j * a_dim1], Abs (d__1));
	  /* L10: */
	}
      if (alpha > anorm)
	{
	  anorm = alpha;
	}
      /* L20: */
    }
  if (anorm == 0.)
    {
      /*    null matrix special case (Serge Steer 96) 
       */
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  nsp_dset (n, &c_b6, &ea[j + ea_dim1], iea);
	  ea[j + j * ea_dim1] = 1.;
	  /* L21: */
	}
      return 0;
    }
  anorm = Max (anorm, 1.);
  /* 
   *call bdiag whith rmax equal to the norm one of matrix a. 
   * 
   */
  nsp_ctrlpack_bdiag (ia, n, &a[a_offset], &c_b6, &anorm, &w[ker], &w[kei],
		      &iw[kbs], &w[kx], &w[kxi], &w[kscal], &c__1, &fail);
  if (fail)
    {
      goto L120;
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      nsp_dset (n, &c_b6, &ea[j + ea_dim1], iea);
      /* L25: */
    }
  /* 
   * compute the pade's approximants of the block. 
   *block origin is shifted before calling pade. 
   * 
   */
  ni = 1;
  k = 0;
  /* 
   * loop on the block. 
   * 
   */
 L30:
  k += ni;
  if (k > *n)
    {
      goto L100;
    }
  ni = iw[kbs - 1 + k];
  if (ni == 1)
    {
      goto L90;
    }
  nii = k + ni - 1;
  bvec = zero;
  i__1 = nii;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      bvec += w[ker - 1 + i__];
      /* L40: */
    }
  bvec /= (double) ni;
  i__1 = nii;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      w[ker - 1 + i__] -= bvec;
      a[i__ + i__ * a_dim1] -= bvec;
      /* L50: */
    }
  alpha = zero;
  i__1 = nii;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      /*Computing 2nd power 
       */
      d__1 = w[ker - 1 + i__];
      /*Computing 2nd power 
       */
      d__2 = w[kei - 1 + i__];
      rn = d__1 * d__1 + d__2 * d__2;
      rn = sqrt (rn);
      if (rn > alpha)
	{
	  alpha = rn;
	}
      /* L60: */
    }
  /* 
   *call pade subroutine. 
   * 
   */
  nsp_ctrlpack_pade (&a[k + k * a_dim1], ia, &ni, &ea[k + k * ea_dim1], iea,
		     &alpha, &w[kw], &iw[kiw], ierr);
  if (*ierr < 0)
    {
      goto L130;
    }
  /* 
   *remove the effect of origin shift on the block. 
   * 
   */
  bbvec = exp (bvec);
  i__1 = nii;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      i__2 = nii;
      for (j = k; j <= i__2; ++j)
	{
	  ea[i__ + j * ea_dim1] *= bbvec;
	  /* L70: */
	}
      /* L80: */
    }
  goto L30;
 L90:
  ea[k + k * ea_dim1] = exp (a[k + k * a_dim1]);
  goto L30;
  /* 
   *end of loop. 
   * 
   */
 L100:
  /* 
   *remove the effect of block diagonalization. 
   * 
   */
  nsp_calpack_dmmul (&w[kx], ia, &ea[ea_offset], iea, &w[kw], n, n, n, n);
  nsp_calpack_dmmul (&w[kw], n, &w[kxi], ia, &ea[ea_offset], iea, n, n, n);
  /* 
   *remove the effects of balance 
   * 
   * 
   *error output 
   * 
   */
  goto L130;
 L110:
  *ierr = -1;
  goto L130;
 L120:
  *ierr = -2;
 L130:
  return 0;
}

