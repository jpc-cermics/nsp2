/* zffti.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "fftpack.h"

/* Subroutine */ int
fftpack_zffti (int *n, double *wsave)
{
  int iw1, iw2;

  /* Parameter adjustments */
  --wsave;

  /* Function Body */
  if (*n == 1)
    {
      return 0;
    }
  iw1 = *n + *n + 1;
  iw2 = iw1 + *n + *n;
  fftpack_cffti1 (n, &wsave[iw1], (int *) &wsave[iw2]);
  return 0;
}				/* zffti_ */
