#include "fftpack.h"

int fftpack_zfftf (int *n, doubleC *c__, double *wsave)
{
  int iw1, iw2;
  /* Parameter adjustments */
  --wsave;
  --c__;
  if (*n == 1)
    {
      return 0;
    }
  iw1 = *n + *n + 1;
  iw2 = iw1 + *n + *n;
  fftpack_cfftf1 (n, (double*) &c__[1], &wsave[1], &wsave[iw1], (int *) &wsave[iw2]);
  return 0;
}

