/* sdot.f -- translated by f2c (version 19961017).
   
	
*/

#include "grand.h"

double
rand_sdot (int *n, double *sx, int *incx, double *sy, int *incy)
{
  /* System generated locals */
  int i__1;
  double ret_val;

  /* Local variables */
  int i__, m;
  double stemp;
  int ix, iy, mp1;

  /* Parameter adjustments */
  --sy;
  --sx;

  /* Function Body */
  stemp = 0.;
  ret_val = 0.;
  if (*n <= 0)
    {
      return ret_val;
    }
  if (*incx == 1 && *incy == 1)
    {
      goto L20;
    }
  ix = 1;
  iy = 1;
  if (*incx < 0)
    {
      ix = (-(*n) + 1) * *incx + 1;
    }
  if (*incy < 0)
    {
      iy = (-(*n) + 1) * *incy + 1;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      stemp += sx[ix] * sy[iy];
      ix += *incx;
      iy += *incy;
/* L10: */
    }
  ret_val = stemp;
  return ret_val;
L20:
  m = *n % 5;
  if (m == 0)
    {
      goto L40;
    }
  i__1 = m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      stemp += sx[i__] * sy[i__];
/* L30: */
    }
  if (*n < 5)
    {
      goto L60;
    }
L40:
  mp1 = m + 1;
  i__1 = *n;
  for (i__ = mp1; i__ <= i__1; i__ += 5)
    {
      stemp =
	stemp + sx[i__] * sy[i__] + sx[i__ + 1] * sy[i__ + 1] + sx[i__ +
								   2] *
	sy[i__ + 2] + sx[i__ + 3] * sy[i__ + 3] + sx[i__ + 4] * sy[i__ + 4];
/* L50: */
    }
L60:
  ret_val = stemp;
  return ret_val;
}				/* sdot_ */
