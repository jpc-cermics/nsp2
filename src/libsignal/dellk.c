#include "signal.h"

/*
 * calculate complete elliptic integral of first kind 
 */

static double c_b3 = 2.;

double
signal_dellk (double *dk)
{
  static double de = 1.;
  int i1;
  double ret_val;
  double dgeo, flma, dari, domi, dtest;
  double dpi, dri;
  
  i1 = (int) C2F(slamch) ("l", 1L) - 2;
  flma = nsp_pow_di (c_b3, i1);
  dpi = atan (1.) * 4.;
  domi = nsp_dlamch ("p") * 2.;
  dgeo = de - *dk * *dk;
  if (dgeo <= 0.)
    {
      goto L10;
    }
  else
    {
      goto L20;
    }
 L10:
  ret_val = flma;
  return ret_val;
 L20:
  dgeo = sqrt (dgeo);
  dri = de;
 L30:
  dari = dri;
  dtest = dari * domi;
  dri = dgeo + dri;
  if (dari - dgeo - dtest <= 0.)
    {
      goto L50;
    }
  else
    {
      goto L40;
    }
 L40:
  dgeo = sqrt (dari * dgeo);
  dri *= .5;
  goto L30;
 L50:
  ret_val = dpi / dri;
  return ret_val;
}

