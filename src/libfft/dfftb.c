/* dfftb.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "fftpack.h"

/* Subroutine */ int
fftpack_dfftb (int *n, double *r__, double *wsave)
{

  /* Parameter adjustments */
  --wsave;
  --r__;

  /* Function Body */
  if (*n == 1)
    {
      return 0;
    }
  fftpack_rfftb1 (n, &r__[1], &wsave[1], &wsave[*n + 1],
		  (int *) &wsave[(*n << 1) + 1]);
  return 0;
}				/* dfftb_ */
