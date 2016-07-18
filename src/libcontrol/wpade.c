/* wpade.f -- translated by f2c (version 19961017).
 */

#include "ctrlpack.h"

/* 
 *!purpose 
 *     compute the pade approximants of the exponential of a complex 
 *     matrix a. we scale a until the spectral radius of a*2**-m 
 *     are smaler than one. 
 * 
 *!calling sequence 
 * 
 *    subroutine wpade(ar,ai,ia,n,ear,eai,iea,alpha,w,ipvt,ierr) 
 * 
 *    int ia,n,iea,ipvt,ierr 
 *    double precision ar,ai,ear,eai,alpha,w 
 *    dimension ar(ia,n),ai(ia,n),ear(iea,n),eai(iea,n),w(*),ipvt(*) 
 * 
 *     ar,ai     : array containing the matrix a 
 *     ia        : the leading dimension of arrays a. 
 *     n         : the order of the matrices a,ea . 
 *     ear,eai   : the  array that contains the n*n 
 *                 matrix exp(a). 
 *     iea       : the leading dimension of array ea. 
 *     alpha     : variable containing the maximun 
 *                 norm of the eigenvalues of a. 
 *     w        : workspace array of size 4*n +4*n*n 
 *     ipvt      : int workspace of size n 
 *     ierr      : error indicator 
 *                 ierr= 0 if normal return 
 *                     =-4 if alpha is to big for any accuracy. 
 * 
 *    common /dcoeff/ c, ndng 
 *    double precision c(41) 
 *    int ndng 
 * 
 *    c          : array containing on return pade coefficients 
 *    ndng       : on first call ndng must be set to -1,on return 
 *               contains degree of pade approximant 
 * 
 *!auxiliary routines 
 *      wclmat  coef wcerr (j. roche) 
 *      wmmul dmcopy (blas.extension) 
 *      wgeco wgesl (linpack.extension) 
 *      sqrt (fortran) 
 *! 
 * 
 * 
 *internal variables 
 * 
 */

#define dcoeff_1 pade_dcoeff

/* Table of constant values */

static int c__0 = 0;

int nsp_ctrlpack_wpade (double *ar, double *ai, int *ia, int *n, double *ear,
			double *eai, int *iea, double *alpha, double *w,
			int *ipvt, int *ierr)
{
  static double zero = 0.;
  static double one = 1.;
  static double two = 2.;
  static int maxc = 10;

  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, ear_dim1, ear_offset, eai_dim1,
    eai_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  double norm;
  int i__, j, k, m;
  double efact;
  double rcond;
  int n2;
  int ki, kr, kw;

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
  --ipvt;

  /* Function Body */
  n2 = *n * *n;
  kr = 1;
  ki = kr + n2;
  kw = ki + n2;
  /* 
   */
  if (dcoeff_1.n >= 0)
    {
      goto L10;
    }
  /* 
   *compute de pade's approximants type which is necessary to obtain 
   *machine precision 
   * 
   */
  nsp_ctrlpack_coef (ierr);
  if (*ierr != 0)
    {
      goto L170;
    }
 L10:
  m = 0;
  efact = one;
  if (*alpha <= 1.)
    {
      goto L90;
    }
  i__1 = maxc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++m;
      efact *= two;
      if (*alpha <= efact)
	{
	  goto L60;
	}
      /* L20: */
    }
  *ierr = -4;
  goto L170;
 L30:
  ++m;
  efact *= two;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  ar[i__ + j * ar_dim1] /= two;
	  ai[i__ + j * ai_dim1] /= two;
	  /* L40: */
	}
      /* L50: */
    }
  norm /= two;
  goto L115;
  /* 
   *we find a matrix a'=a*2-m whith a spectral radius smaller than one. 
   * 
   */
 L60:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  ar[i__ + j * ar_dim1] /= efact;
	  ai[i__ + j * ai_dim1] /= efact;
	  /* L70: */
	}
      /* L80: */
    }
 L90:
  /* 
   * 
   */
  nsp_ctrlpack_wcerr (&ar[ar_offset], &ai[ai_offset], &w[1], ia, n,
		      &dcoeff_1.n, &m, &maxc);
  /* 
   * 
   */
  norm = zero;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      *alpha = zero;
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  *alpha = *alpha + (d__1 =
			     ar[i__ + j * ar_dim1], Abs (d__1)) + (d__2 =
								   ai[i__ +
								      j *
								      ai_dim1],
								   Abs
								   (d__2));
	  /* L100: */
	}
      if (*alpha > norm)
	{
	  norm = *alpha;
	}
      /* L110: */
    }
  /* 
   *compute the inverse of the denominator of dpade's approximants. 
   * 
   */
 L115:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  ear[i__ + j * ear_dim1] = -ar[i__ + j * ar_dim1];
	  eai[i__ + j * eai_dim1] = -ai[i__ + j * ai_dim1];
	  /* L120: */
	}
      /* L130: */
    }
  nsp_ctrlpack_wclmat (iea, n, &ear[ear_offset], &eai[eai_offset], &w[kr],
		       &w[ki], n, &w[kw], dcoeff_1.b, &dcoeff_1.n);
  /* 
   *compute de l-u decomposition of n (-a) and the condition number 
   *                                 pp 
   * 
   */
  nsp_ctrlpack_wgeco (&w[kr], &w[ki], n, n, &ipvt[1], &rcond, &w[kw],
		      &w[kw + *n]);
  /* 
   *Computing 4th power 
   */
  d__1 = rcond, d__1 *= d__1;
  rcond = d__1 * d__1;
  if (rcond + one <= one && (norm > one && m < maxc))
    {
      goto L30;
    }
  /* 
   *compute the numerator of dpade's approximants. 
   * 
   */
  nsp_ctrlpack_wclmat (ia, n, &ar[ar_offset], &ai[ai_offset],
		       &ear[ear_offset], &eai[eai_offset], iea, &w[kw],
		       dcoeff_1.b, &dcoeff_1.n);
  /* 
   *compute the dpade's approximants by 
   * 
   *     n (-a) x=n (a) 
   *     pp      pp 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      nsp_ctrlpack_wgesl (&w[kr], &w[ki], n, n, &ipvt[1],
			  &ear[j * ear_dim1 + 1], &eai[j * eai_dim1 + 1],
			  &c__0);
      /* L150: */
    }
  if (m == 0)
    {
      goto L170;
    }
  /* 
   *remove the effects of normalization. 
   * 
   */
  i__1 = m;
  for (k = 1; k <= i__1; ++k)
    {
      nsp_calpack_wmmul (&ear[ear_offset], &eai[eai_offset], iea,
			 &ear[ear_offset], &eai[eai_offset], iea, &w[kr],
			 &w[ki], n, n, n, n);
      nsp_calpack_dmcopy (&w[kr], n, &ear[ear_offset], iea, n, n);
      nsp_calpack_dmcopy (&w[ki], n, &eai[eai_offset], iea, n, n);
      /* L160: */
    }
 L170:
  return 0;
}				/* wpade_ */
