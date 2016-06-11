/* zlog.f -- translated by f2c (version 19961017). */

#include "calpack.h"

int nsp_calpack_zlog (double *ar, double *ai, double *br, double *bi, int *ierr)
{
  double zm, dtheta;
  
  *ierr = 0;
  if (*ar == 0.)
    {
      goto L10;
    }
  if (*ai == 0.)
    {
      goto L20;
    }
  dtheta = atan (*ai / *ar);
  if (dtheta <= 0.)
    {
      goto L40;
    }
  if (*ar < 0.)
    {
      dtheta -= M_PI;
    }
  goto L50;
 L10:
  if (*ai == 0.)
    {
      goto L60;
    }
  *bi = M_PI_2;
  *br = log ((Abs (*ai)));
  if (*ai < 0.)
    {
      *bi = -(*bi);
    }
  return 0;
 L20:
  if (*ar > 0.)
    {
      goto L30;
    }
  *br = log ((Abs (*ar)));
  *bi = M_PI;
  return 0;
 L30:
  *br = log (*ar);
  *bi = 0.;
  return 0;
 L40:
  if (*ar < 0.)
    {
      dtheta += M_PI;
    }
 L50:
  zm = nsp_calpack_zabs (ar, ai);
  *br = log (zm);
  *bi = dtheta;
  return 0;
 L60:
  *ierr = 1;
  return 0;
}

