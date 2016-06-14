/* dffti.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "fftpack.h"

/* Subroutine */ int
fftpack_dffti (int *n, double *wsave)
{

  /* Parameter adjustments */
  --wsave;

  /* Function Body */
  if (*n == 1)
    {
      return 0;
    }
  fftpack_rffti1 (n, &wsave[*n + 1], (int *) &wsave[(*n << 1) + 1]);
  return 0;
}				/* dffti_ */
