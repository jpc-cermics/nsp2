/* pade.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

#define dcoeff_1 pade_dcoeff


/* Table of constant values */

static int c__0 = 0;

int
nsp_ctrlpack_pade (double *a, int *ia, int *n, double *ea, int *iea,
		   double *alpha, double *wk, int *ipvt, int *ierr)
{
  /* Initialized data */

  static double zero = 0.;
  static double one = 1.;
  static double two = 2.;
  static int maxc = 10;

  /* System generated locals */
  int a_dim1, a_offset, ea_dim1, ea_offset, i__1, i__2;
  double d__1;

  /* Local variables */
  double norm;
  int i__, j, k, m;
  double efact;
  double rcond;
  int n2;

  /* 
   *!purpose 
   *     compute the pade approximants of the exponential of a 
   *     matrix a. we scale a until the spectral radius of a*2**-m 
   *     are smaler than one. 
   * 
   *!calling sequence 
   * 
   *    subroutine pade(a,ia,n,ea,iea,alpha,wk,ipvt,ierr) 
   * 
   *    int ia,n,iea,ipvt,ierr 
   *    double precision a,ea,alpha,wk, 
   *    dimension a(ia,*),ea(iea,*),wk(*),ipvt(*) 
   * 
   *     a         : array containing the matrix a 
   *     ia        : the leading dimension of arrays a. 
   *     n         : the order of the matrices a,ea . 
   *     ea        : the  array that contains the n*n 
   *                 matrix exp(a). 
   *     iea       : the leading dimension of array ea. 
   *     alpha     : variable containing the maximun 
   *                 norm of the eigenvalues of a. 
   *     wk        : workspace array of size 2*n*(n+1) 
   *     ipvt      : int workspace of size n 
   *     ierr      : error indicator 
   *                 ierr= 0 if normal return 
   *                     =-4 if alpha is to big for any accuracy. 
   * 
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
   *      dclmat  coef cerr (j. roche) 
   *      dmmul dmcopy (blas.extension) 
   *      dgeco dgesl (linpack) 
   *      sqrt (fortran) 
   *! 
   * 
   *internal variables 
   * 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *ia;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  ea_dim1 = *iea;
  ea_offset = ea_dim1 + 1;
  ea -= ea_offset;
  --wk;
  --ipvt;

  /* Function Body */
  n2 = *n * *n;
  /* 
   */
  if (dcoeff_1.n >= 0)
    {
      goto L10;
    }
  /* 
   *compute de pade's aprroximants type which is necessary to obtain 
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
	  a[i__ + j * a_dim1] /= two;
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
	  a[i__ + j * a_dim1] /= efact;
	  /* L70: */
	}
      /* L80: */
    }
 L90:
  /* 
   * 
   */
  nsp_ctrlpack_cerr (&a[a_offset], &wk[1], ia, n, &dcoeff_1.n, &m, &maxc);
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
	  *alpha += (d__1 = a[i__ + j * a_dim1], Abs (d__1));
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
	  ea[i__ + j * ea_dim1] = -a[i__ + j * a_dim1];
	  /* L120: */
	}
      /* L130: */
    }
  nsp_ctrlpack_dclmat (iea, n, &ea[ea_offset], &wk[1], n, &wk[n2 + 1],
		       dcoeff_1.b, &dcoeff_1.n);
  /* 
   *compute de l-u decomposition of n (-a) and the condition numbwk(n2+1) 
   *                                 pp 
   * 
   */
  nsp_ctrlpack_dgeco (&wk[1], n, n, &ipvt[1], &rcond, &wk[n2 + 1]);
  /* 
   */
  rcond = rcond * rcond * rcond * rcond;
  if (rcond + one <= one && (norm > one && m < maxc))
    {
      goto L30;
    }
  /* 
   *compute the numerator of dpade's approximants. 
   * 
   */
  nsp_ctrlpack_dclmat (ia, n, &a[a_offset], &ea[ea_offset], iea, &wk[n2 + 1],
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
      nsp_ctrlpack_dgesl (&wk[1], n, n, &ipvt[1], &ea[j * ea_dim1 + 1],
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
      nsp_calpack_dmmul (&ea[ea_offset], iea, &ea[ea_offset], iea, &wk[1], n,
			 n, n, n);
      nsp_calpack_dmcopy (&wk[1], n, &ea[ea_offset], iea, n, n);
      /* L160: */
    }
 L170:
  return 0;
}

