/* vpythag.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int nsp_calpack_vpythag (int *n, double *xr, double *xi)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;

  /* 
   *    xr(i) = pythag(xr(i),xi(i)) 
   *    Copyright INRIA 
   */
  /* Parameter adjustments */
  --xi;
  --xr;

  /* Function Body */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      xr[i__] = nsp_calpack_pythag (&xr[i__], &xi[i__]);
      /* L10: */
    }
  return 0;
}				/* vpythag_ */
