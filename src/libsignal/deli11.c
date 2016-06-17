#include "signal.h"

/*
 * elliptic function 
 */

int signal_deli11 (const double *x,const double *ck, double *res)
{
  double d__1;

  /* Local variables */
  double aari, domi, cres, test, angle, sqgeo;
  double geo, ari, dpi, pim;

  dpi = atan (1.) * 4.;
  domi = nsp_dlamch ("p") * 2.;
  if (*x == 0.)
    {
      goto L10;
    }
  else
    {
      goto L20;
    }
 L10:
  *res = 0.;
  return 0;
 L20:
  if (*ck == 0.)
    {
      goto L30;
    }
  else
    {
      goto L40;
    }
 L30:
  *res = log (Abs (*x) + sqrt (*x * *x + 1.));
  goto L130;
 L40:
  angle = (d__1 = 1. / *x, Abs (d__1));
  geo = Abs (*ck);
  ari = 1.;
  pim = 0.;
 L50:
  sqgeo = ari * geo;
  aari = ari;
  ari = geo + ari;
  angle = -sqgeo / angle + angle;
  sqgeo = sqrt (sqgeo);
  if (angle == 0.)
    {
      goto L60;
    }
  else
    {
      goto L70;
    }
  /* replace 0 by a small value, test 
   */
 L60:
  angle = sqgeo * domi;
 L70:
  test = aari * domi * 1e5;
  cres = (d__1 = aari - geo, Abs (d__1)) - test;
  if (cres <= 0.)
    {
      goto L100;
    }
  else
    {
      goto L80;
    }
 L80:
  geo = sqgeo + sqgeo;
  pim += pim;
  if (angle < 0.)
    {
      goto L90;
    }
  else
    {
      goto L50;
    }
 L90:
  pim += dpi;
  goto L50;
 L100:
  if (angle < 0.)
    {
      goto L110;
    }
  else
    {
      goto L120;
    }
 L110:
  pim += dpi;
 L120:
  *res = (atan (ari / angle) + pim) / ari;
 L130:
  if (*x < 0.)
    {
      goto L140;
    }
  else
    {
      goto L150;
    }
 L140:
  *res = -(*res);
 L150:
  return 0;
}

