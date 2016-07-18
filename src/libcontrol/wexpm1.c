/* wexpm1.f -- translated by f2c (version 19961017).
 */

/* 
 *!purpose 
 *     compute the exponential of a complex matrix a by the pade's 
 *     approximants(subroutine pade).a block diagonalization 
 *        is performed prior call pade. 
 *!calling sequence 
 *    subroutine wexpm1(n,ar,ai,ia,ear,eai,iea,w,iw,ierr) 
 * 
 *    int ia,n,iw,ierr 
 *    double precision ar,ai,ear,eai,w 
 *    dimension ar(ia,n),ai(ia,n),ear(iea,n),eai(iea,n),w(*),iw(*) 
 * 
 *     n: the order of the matrices a,ea, . 
 *     ar,ai :the  array that contains :the n*n matrix a 
 *     ia: the leading dimension of array a. 
 *     ear,eai: the array that contains the n*n exponential of a. 
 *     iea    :the leading dimension of ea 
 *     w : work space array of size: n*(4*ia+4*n+7) 
 *     iw : int work space array of size 2*n 
 *     ierr: =0 if the prosessus is normal. 
 *              =-1 if n>ia. 
 *              =-2 if the block diagonalization is not possible. 
 *              =-4 if the subroutine dpade can not computes exp(a) 
 * 
 *!auxiliary routines 
 *    cos sin exp abs sqrt dble real (fortran) 
 *    wbdiag wbalin (eispack.extension) 
 *    cbal (eispack) 
 *    wmmul (blas.extension) 
 *    wpade 
 *! originator 
 *    S Steer INRIA  from dexpm1: 
 *    j roche laboratoire d'automatique de grenoble 
 *! 
 *    Copyright INRIA 
 *internal variables 
 * 
 * 
 * 
 */

#include "ctrlpack.h"

#define dcoeff_1 pade_dcoeff

/* Table of constant values */

static double c_b6 = 0.;
static int c__1 = 1;

int
nsp_ctrlpack_wexpm1 (int *n, double *ar, double *ai, int *ia, double *ear,
		     double *eai, int *iea, double *w, int *iw, int *ierr)
{
  /* Initialized data */

  static double zero = 0.;

  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, ear_dim1, ear_offset, eai_dim1,
    eai_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int fail;
  int kpvt, i__, j, k;
  double bbvec, alpha, bveci;
  int kscal;
  double bvecr, anorm;
  int ni, nn;
  double rn;
  int kw, kei, nii, kbs, ker, kxi, kyi, kxr, kyr;

  /* Parameter adjustments */
  ai_dim1 = *ia;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *ia;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  eai_dim1 = *iea;
  eai_offset = eai_dim1 + 1;
  eai -= eai_offset;
  ear_dim1 = *iea;
  ear_offset = ear_dim1 + 1;
  ear -= ear_offset;
  --w;
  --iw;

  /* Function Body */
  dcoeff_1.n = -1;
  /* 
   */
  *ierr = 0;
  nn = *n * *n;
  kscal = 1;
  kxr = kscal + *n;
  kxi = kxr + *n * *ia;
  kyr = kxi + *n * *ia;
  kyi = kyr + *n * *ia;
  ker = kyi + *n * *ia;
  kei = ker + *n;
  kw = kei + *n;
  /* 
   */
  kbs = 1;
  kpvt = kbs + *n;
  /* 
   */
  if (*n > *ia)
    {
      goto L110;
    }
  /* 
   * compute the norm one of a. 
   * 
   */
  anorm = 0.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      alpha = zero;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  alpha = alpha + (d__1 = ar[i__ + j * ar_dim1], Abs (d__1)) + (d__2 =
									ai[i__
									   +
									   j *
									   ai_dim1],
									Abs
									(d__2));
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
	  nsp_dset (n, &c_b6, &ear[j + ear_dim1], iea);
	  nsp_dset (n, &c_b6, &eai[j + eai_dim1], iea);
	  ear[j + j * ear_dim1] = 1.;
	  /* L21: */
	}
      return 0;
    }
  anorm = Max (anorm, 1.);
  /* 
   *call wbdiag whith rmax equal to the norm one of matrix a. 
   * 
   */
  nsp_ctrlpack_wbdiag (ia, n, &ar[ar_offset], &ai[ai_offset], &anorm, &w[ker],
		       &w[kei], &iw[kbs], &w[kxr], &w[kxi], &w[kyr], &w[kyi],
		       &w[kscal], &c__1, &fail);
  if (fail)
    {
      goto L120;
    }
  /* 
   *clear matrix ea 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      nsp_dset (n, &c_b6, &ear[j + ear_dim1], iea);
      nsp_dset (n, &c_b6, &eai[j + eai_dim1], iea);
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
  bvecr = zero;
  bveci = zero;
  i__1 = nii;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      bvecr += w[ker - 1 + i__];
      bveci += w[kei - 1 + i__];
      /* L40: */
    }
  bvecr /= (double) ni;
  bveci /= (double) ni;
  i__1 = nii;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      w[ker - 1 + i__] -= bvecr;
      w[kei - 1 + i__] -= bveci;
      ar[i__ + i__ * ar_dim1] -= bvecr;
      ai[i__ + i__ * ai_dim1] -= bveci;
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
  nsp_ctrlpack_wpade (&ar[k + k * ar_dim1], &ai[k + k * ai_dim1], ia, &ni,
		      &ear[k + k * ear_dim1], &eai[k + k * eai_dim1], iea,
		      &alpha, &w[kw], &iw[kpvt], ierr);
  if (*ierr < 0)
    {
      goto L130;
    }
  /* 
   *remove the effect of origin shift on the block. 
   * 
   */
  bbvec = exp (bvecr);
  bvecr = bbvec * cos (bveci);
  bveci = bbvec * sin (bveci);
  i__1 = nii;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      i__2 = nii;
      for (j = k; j <= i__2; ++j)
	{
	  bbvec =
	    ear[i__ + j * ear_dim1] * bvecr - eai[i__ + j * eai_dim1] * bveci;
	  eai[i__ + j * eai_dim1] =
	    ear[i__ + j * ear_dim1] * bveci + eai[i__ + j * eai_dim1] * bvecr;
	  ear[i__ + j * ear_dim1] = bbvec;
	  /* L70: */
	}
      /* L80: */
    }
  goto L30;
 L90:
  bbvec = exp (ar[k + k * ar_dim1]);
  ear[k + k * ear_dim1] = bbvec * cos (ai[k + k * ai_dim1]);
  eai[k + k * eai_dim1] = bbvec * sin (ai[k + k * ai_dim1]);
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
  nsp_calpack_wmmul (&w[kxr], &w[kxi], ia, &ear[ear_offset],
		     &eai[eai_offset], iea, &w[kw], &w[kw + nn], n, n, n, n);
  nsp_calpack_wmmul (&w[kw], &w[kw + nn], n, &w[kyr], &w[kyi], ia,
		     &ear[ear_offset], &eai[eai_offset], iea, n, n, n);
  /* 
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
}				/* wexpm1_ */
