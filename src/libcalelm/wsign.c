
#include "calpack.h"

int
nsp_calpack_wsign (double *xr, double *xi, double *yr, double *yi, double *zr, double *zi)
{
  double d__1, d__2, t;

  t = nsp_calpack_pythag (yr, yi);
  *zr = *xr;
  *zi = *xi;
  if (t != 0.)
    {
      d__1 = *yr / t;
      d__2 = *yi / t;
      nsp_calpack_wmul (&d__1, &d__2, zr, zi, zr, zi);
    }
  return 0;
}

