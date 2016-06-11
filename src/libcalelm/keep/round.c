/* round.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

double nsp_calpack_sciround (double *x1)
{
  /* Initialized data */

  static double h__ = 1e9;

  /* System generated locals */
  double ret_val;

  /* Local variables */
  double e, x, y, z__;

  /*! 
   *    Copyright INRIA 
   */
  /* 
   */
  x = *x1;
  if (x == 0.)
    {
      ret_val = x;
      return ret_val;
    }
  if (x * 2. == (double) ((int) (x * 2.)))
    {
      /*    changing the signs gives round(0.5)=0 round(-0.5)=0 
       */
      if (x > 0.)
	{
	  x += 1e-10;
	}
      if (x < 0.)
	{
	  x += -1e-10;
	}
    }
  z__ = Abs (x);
  /*    -----testing Nans 
   */
  if (nsp_calpack_isanan (&x) == 1)
    {
      ret_val = x;
      return ret_val;
    }
  y = z__ + 1.;
  if (y == z__)
    {
      ret_val = x;
      return ret_val;
    }
  y = 0.;
  e = h__;
L10:
  if (e >= z__)
    {
      goto L20;
    }
  e *= 2.;
  goto L10;
L20:
  if (e <= h__)
    {
      goto L30;
    }
  if (e <= z__)
    {
      y += e;
    }
  if (e <= z__)
    {
      z__ -= e;
    }
  e /= 2.;
  goto L20;
L30:
  z__ = (double) ((int) (z__ + .5));
  y += z__;
  if (x < 0.)
    {
      y = -y;
    }
  ret_val = y;
  return ret_val;
}				/* sciround_ */
#endif
