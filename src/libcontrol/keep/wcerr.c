/* wcerr.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static double c_b6 = 0.;
static int c__1 = 1;
static double c_b8 = 1.;

/*/MEMBR ADD NAME=WCERR,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_ctrlpack_wcerr (double *ar, double *ai, double *w, int *ia, int *n,
		    int *ndng, int *m, int *maxc)
{
  /* Initialized data */

  static double zero = 0.;
  static double one = 1.;
  static double two = 2.;

  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Local variables */
  int itab[15];
  double norm=0, norm1;
  int i__, j, k, l;
  double alpha;
  int i1, n2;
  int ki, mm, kr, mt;
  int kei, ker, kwi, kwr;

  /*!purpose 
   *    wcerr evaluate the error introduced by pade 
   *    approximant and normalise the complex matrix a accordingly 
   *!calling sequence 
   * 
   *    subroutine wcerr(ar,ai,w,ia,n,ndng,m,maxc) 
   * 
   *    ar,ai    : array containing the matrix a 
   * 
   *    w        : work space array of size 4*n*n + 2*n 
   * 
   *    ia       : leading dimension of array a 
   * 
   *    n        : size of matrix a 
   * 
   *    ndng     : degree of pade approximant 
   * 
   *    m        :  the factor of normalisation is 2**(-m) 
   * 
   *    maxc     : maximum admissible for m 
   * 
   *!auxiliary routines 
   *    wmmul dmcopy  gdcp2i (blas.extension) 
   *    dset dcopy (blas) 
   *    ddot (blas) 
   *    abs real dble (fortran) 
   *! 
   *    w  tableau de travail de taille 4*n*n+2*n 
   * 
   * 
   *internal variables 
   * 
   */
  /* Parameter adjustments */
  --w;
  ai_dim1 = *ia;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *ia;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;

  /* Function Body */
  /* 
   * 
   */
  n2 = *n * *n;
  kr = 1;
  ki = kr + n2;
  ker = ki + n2;
  kei = ker + n2;
  kwr = kei + n2;
  kwi = kwr + *n;
  k = *ndng << 1;
  nsp_calpack_wmmul (&ar[ar_offset], &ai[ai_offset], ia, &ar[ar_offset],
		     &ai[ai_offset], ia, &w[ker], &w[kei], n, n, n, n);
  nsp_calpack_gdcp2i (&k, itab, &mt);
  if (!itab[0])
    {
      goto L30;
    }
  norm = zero;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      alpha = zero;
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
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
      if (alpha > norm)
	{
	  norm = alpha;
	}
      /* L20: */
    }
  nsp_calpack_dmcopy (&ar[ar_offset], ia, &w[kr], n, n, n);
  nsp_calpack_dmcopy (&ai[ai_offset], ia, &w[ki], n, n, n);
  goto L40;
 L30:
  nsp_dset (&n2, &c_b6, &w[kr], &c__1);
  i__1 = *n + 1;
  nsp_dset (n, &c_b8, &w[kr], &i__1);
  nsp_dset (&n2, &c_b6, &w[ki], &c__1);
 L40:
  if (mt == 1)
    {
      goto L110;
    }
  i__1 = mt;
  for (i1 = 2; i1 <= i__1; ++i1)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  l = 0;
	  i__3 = *n;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      w[kwr - 1 + i__] =
		C2F (ddot) (n, &w[kr - 1 + j], n, &w[ker + l],
			    &c__1) - C2F (ddot) (n,
						 &w[ki - 1 + j],
						 n, &w[kei + l], &c__1);
	      w[kwi - 1 + i__] =
		C2F (ddot) (n, &w[kr - 1 + j], n, &w[kei + l],
			    &c__1) + C2F (ddot) (n,
						 &w[ki - 1 + j],
						 n, &w[ker + l], &c__1);
	      l += *n;
	      /* L50: */
	    }
	  C2F (dcopy) (n, &w[kwr], &c__1, &w[kr - 1 + j], n);
	  C2F (dcopy) (n, &w[kwi], &c__1, &w[ki - 1 + j], n);
	  /* L70: */
	}
      if (!itab[i1 - 1])
	{
	  goto L100;
	}
      norm1 = zero;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  alpha = zero;
	  l = i__ - 1;
	  i__3 = *n;
	  for (j = 1; j <= i__3; ++j)
	    {
	      alpha = alpha + (d__1 = w[kr + l], Abs (d__1)) + (d__2 =
								w[ki + l],
								Abs (d__2));
	      l += *n;
	      /* L80: */
	    }
	  if (alpha > norm1)
	    {
	      norm1 = alpha;
	    }
	  /* L90: */
	}
      norm *= norm1;
    L100:
      ;
    }
 L110:
  norm /= (double) (k + 1);
  i__1 = *ndng;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /*Computing 2nd power 
       */
      i__2 = k - i__ + 1;
      norm /= (double) (i__2 * i__2);
      /* L120: */
    }
  norm *= 8.;
  mm = 0;
 L130:
  if (norm + one <= one)
    {
      goto L140;
    }
  ++mm;
  alpha = pow_di (two, mm);
  norm /= alpha;
  if (mm + *m > *maxc)
    {
      goto L140;
    }
  goto L130;
 L140:
  alpha = pow_di (two, mm);
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  ar[i__ + j * ar_dim1] /= alpha;
	  ai[i__ + j * ai_dim1] /= alpha;
	  /* L150: */
	}
      /* L160: */
    }
  *m += mm;
  return 0;
}				/* wcerr_ */
