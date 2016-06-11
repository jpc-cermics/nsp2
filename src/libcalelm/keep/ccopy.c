/* ccopy.f -- translated by f2c (version 19961017).
 *
 *
 */

#if 0
#include "calpack.h"

int
nsp_calpack_ccopy (int *n, char *dx, int *incx, char *dy, int *incy,
		   long int dx_len, long int dy_len)
{
  /* System generated locals */
  int i__1, i__2, i__3;

  /* Builtin functions */
  int s_copy (char *, char *, long int, long int);

  /* Local variables */
  int i__, m, ix, iy, mp1;

  /*    same as dcopy but for characters 
   *    Copyright INRIA 
   * 
   */
  if (*n <= 0)
    {
      return 0;
    }
  if (*incx == 1 && *incy == 1)
    {
      goto L20;
    }
  /* 
   *       code for unequal increments or equal increments 
   *         not equal to 1 
   * 
   */
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
      *(unsigned char *) &dy[iy - 1] = *(unsigned char *) &dx[ix - 1];
      ix += *incx;
      iy += *incy;
      /* L10: */
    }
  return 0;
  /* 
   *       code for both increments equal to 1 
   * 
   * 
   *       clean-up loop 
   * 
   */
L20:
  m = *n % 7;
  if (m == 0)
    {
      goto L40;
    }
  i__1 = m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      *(unsigned char *) &dy[i__ - 1] = *(unsigned char *) &dx[i__ - 1];
      /* L30: */
    }
  if (*n < 7)
    {
      return 0;
    }
L40:
  mp1 = m + 1;
  i__1 = *n;
  for (i__ = mp1; i__ <= i__1; i__ += 7)
    {
      *(unsigned char *) &dy[i__ - 1] = *(unsigned char *) &dx[i__ - 1];
      i__2 = i__;
      i__3 = i__;
      s_copy (dy + i__2, dx + i__3, i__ + 1 - i__2, i__ + 1 - i__3);
      i__2 = i__ + 1;
      i__3 = i__ + 1;
      s_copy (dy + i__2, dx + i__3, i__ + 2 - i__2, i__ + 2 - i__3);
      i__2 = i__ + 2;
      i__3 = i__ + 2;
      s_copy (dy + i__2, dx + i__3, i__ + 3 - i__2, i__ + 3 - i__3);
      i__2 = i__ + 3;
      i__3 = i__ + 3;
      s_copy (dy + i__2, dx + i__3, i__ + 4 - i__2, i__ + 4 - i__3);
      i__2 = i__ + 4;
      i__3 = i__ + 4;
      s_copy (dy + i__2, dx + i__3, i__ + 5 - i__2, i__ + 5 - i__3);
      i__2 = i__ + 5;
      i__3 = i__ + 5;
      s_copy (dy + i__2, dx + i__3, i__ + 6 - i__2, i__ + 6 - i__3);
      /* L50: */
    }
  return 0;
}				/* ccopy_ */
#endif
