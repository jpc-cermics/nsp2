/* dzffti.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "fftpack.h"

/* Subroutine */ int
fftpack_dzffti (int *n, double *wsave)
{

  /* Parameter adjustments */
  --wsave;

  /* Function Body */
  if (*n == 1)
    {
      return 0;
    }
  fftpack_ezfft1 (n, &wsave[(*n << 1) + 1], (int *) &wsave[*n * 3 + 1]);
  return 0;
}				/* dzffti_ */
