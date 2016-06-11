/* qzk.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

int nsp_ctrlpack_qzk (double *q, double *a, int *n, int *kmax, double *c__)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  double tamp;
  int i__, k, k1, n1;
  double an;
  int km1;

  /*ce sous programme determine les coefficients constant ck du quotient 
   *de la division euclidienne q*z**k/a pour k variant de 1 a kmax 
   *q polynome de degre n-1 
   *a polynome de degre n 
   *ck=(q(n+1-k)-sum(a(n+1-i)*c(k-i)) )/an i=1...min(n,k-1) 
   *    Copyright INRIA 
   * 
   */
  /* Parameter adjustments */
  --c__;
  --a;
  --q;

  /* Function Body */
  an = a[*n + 1];
  c__[1] = q[*n] / an;
  if (*kmax == 1)
    {
      return 0;
    }
  n1 = *n + 1;
  k1 = Min (*n, *kmax);
  if (*n == 1)
    {
      goto L25;
    }
  i__1 = k1;
  for (k = 2; k <= i__1; ++k)
    {
      km1 = k - 1;
      tamp = 0.;
      i__2 = km1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /* L10: */
	  tamp += a[n1 - i__] * c__[k - i__];
	}
      c__[k] = (q[n1 - k] - tamp) / an;
      /* L20: */
    }
  if (k1 > *kmax)
    {
      return 0;
    }
 L25:
  i__1 = *kmax;
  for (k = n1; k <= i__1; ++k)
    {
      tamp = 0.;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /* L30: */
	  tamp += a[n1 - i__] * c__[k - i__];
	}
      c__[k] = -tamp / an;
      /* L40: */
    }
  return 0;
}				/* qzk_ */
