/* dzfftf.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "fftpack.h"

/* Subroutine */ int
fftpack_dzfftf (int *n, double *r__, double *azero, double *a, double *b,
		double *wsave)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;
  double cf;
  int ns2;
  double cfm;
  int ns2m;

  /* 
   *                      VERSION 3  JUNE 1979 
   * 
   */
  /* Parameter adjustments */
  --wsave;
  --b;
  --a;
  --r__;

  /* Function Body */
  if ((i__1 = *n - 2) < 0)
    {
      goto L101;
    }
  else if (i__1 == 0)
    {
      goto L102;
    }
  else
    {
      goto L103;
    }
 L101:
  *azero = r__[1];
  return 0;
 L102:
  *azero = (r__[1] + r__[2]) * .5;
  a[1] = (r__[1] - r__[2]) * .5;
  return 0;
 L103:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      wsave[i__] = r__[i__];
      /* L104: */
    }
  fftpack_dfftf (n, &wsave[1], &wsave[*n + 1]);
  cf = 2. / (double) (*n);
  cfm = -cf;
  *azero = cf * .5 * wsave[1];
  ns2 = (*n + 1) / 2;
  ns2m = ns2 - 1;
  i__1 = ns2m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      a[i__] = cf * wsave[i__ * 2];
      b[i__] = cfm * wsave[(i__ << 1) + 1];
      /* L105: */
    }
  if (*n % 2 == 1)
    {
      return 0;
    }
  a[ns2] = cf * .5 * wsave[*n];
  b[ns2] = 0.;
  return 0;
}				/* dzfftf_ */
