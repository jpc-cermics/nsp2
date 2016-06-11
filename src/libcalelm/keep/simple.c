/* simple.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int nsp_calpack_simple (int *n, double *d__, double *s)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  double rmax;
  int i__;

  /*! 
   *    Copyright INRIA 
   * 
   */
  /* Parameter adjustments */
  --s;
  --d__;

  /* Function Body */
  rmax = C2F (slamch) ("o", 1L);
  /* 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if ((d__1 = d__[i__], Abs (d__1)) > rmax)
	{
	  d__1 = rmax;
	  s[i__] = d_sign (&d__1, &d__[i__]);
	}
      else
	{
	  s[i__] = d__[i__];
	}
      /* L10: */
    }
  return 0;
}				/* simple_ */
