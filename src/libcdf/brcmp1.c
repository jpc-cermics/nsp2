#include "cdf.h"

static double cdf_esum (int mu,double x);

/**
 * cdf_brcmp1:
 * @mu: an int  
 * @a: a double
 * @b: a double
 * @x: a double
 * @y: a double
 * 
 * evaluation of  exp(mu) * (x^a*y^b/beta(a,b))
 * 
 * Returns: a double. 
 **/

double cdf_brcmp1_new (int mu, double a, double b, double x, double y)
{
  const double one_sqrttpi = .398942280401433;/* 1/SQRT(2*PI)  */
  int i1, i, n;
  double ret_val, d1, c, e, h, t, u, v, z, a0, b0, x0, y0, lambda, apb, lnx, lny;

  a0 = Min (a, b);
  if (a0 >= 8.)
    {
      /* procedure for a .ge. 8 and b .ge. 8 */
      if (a > b)
	{
	  h = b / a;
	  x0 = 1. / (h + 1.);
	  y0 = h / (h + 1.);
	  lambda = (a + b) * y - b;
	}
      else 
	{
	  h = a / b;
	  x0 = h / (h + 1.);
	  y0 = 1. / (h + 1.);
	  lambda = a - (a + b) * x;
	}
      e = -lambda / a;
      if (Abs (e) > .6)
	{
	  u = e - log (x / x0);
	}
      else 
	{
	  u = cdf_rlog1 (e);
	}
      e = lambda / b;
      if (Abs (e) > .6)
	{
	  v = e - log (y / y0);
	}
      else 
	{
	  v = cdf_rlog1 (e);
	}
      d1 = -(a * u + b * v);
      z = cdf_esum (mu, d1);
      return one_sqrttpi * sqrt (b * x0) * z * exp (-cdf_bcorr (a, b));
    }

  if ( x <= .375)
    {
      lnx = log(x);
      lny = cdf_dln1px (-x);
    }
  else 
    {
      if (y <= .375)
	{
	  lnx = cdf_dln1px (-y);
	  lny = log (y);
	}
      else 
	{
	  lnx = log (x);
	  lny = log (y);
	}
    }

  z = a * lnx + b * lny;
  if (a0 < 1.)
    {
      /* procedure for a .lt. 1 or b .lt. 1 */
      b0 = Max (a, b);
      if (b0 >= 8.)
	{
	  /* algorithm for b0 .ge. 8 */
	  u = cdf_gamln1 (a0) + cdf_algdiv (a0, b0);
	  d1 = z - u;
	  return  a0 * cdf_esum (mu, d1);
	}
      if (b0 > 1.)
	{
	  /* algorithm for 1 .lt. b0 .lt. 8 */
	  u = cdf_gamln1 (a0);
	  n = (int) (b0 - 1.);
	  if (n >= 1)
	    {
	      c = 1.;
	      i1 = n;
	      for (i = 1; i <= i1; ++i)
		{
		  b0 += -1.;
		  c *= b0 / (a0 + b0);
		}
	      u = log (c) + u;
	    }
	  z -= u;
	  b0 += -1.;
	  apb = a0 + b0;
	  if (apb > 1.)
	    {
	      u = a0 + b0 - 1.;
	      t = (cdf_gam1 (u) + 1.) / apb;
	    }
	  else 
	    {
	      t = cdf_gam1 (apb) + 1.;
	    }
	  return a0 * cdf_esum (mu, z) * (cdf_gam1 (b0) + 1.) / t;
	}
      /* algorithm for b0 .le. 1 */
      ret_val = cdf_esum (mu, z);
      if (ret_val == 0.)
	{
	  return ret_val;
	}

      apb = a + b;
      if (apb > 1.)
	{
	  u = a + b - 1.;
	  z = (cdf_gam1 (u) + 1.) / apb;
	}
      else 
	{
	  z = cdf_gam1 (apb) + 1.;
	}
      c = (cdf_gam1 (a) + 1.) * (cdf_gam1 (b) + 1.) / z;
      ret_val = ret_val * (a0 * c) / (a0 / b0 + 1.);
      return ret_val;
    }
  z -= cdf_betaln (a, b);
  return cdf_esum (mu, z);
}


double cdf_brcmp1 (int mu, double a, double b, double x, double y)
{
  /*     const = 1/sqrt(2*pi) */
  const double one_sqrttpi = .398942280401433;
  int i1;
  double ret_val, d__1;
  double c__, e, h__;
  int i, n;
  double t, u, v, z;
  double a0, b0, x0, y0;
  double lambda;
  double apb, lnx, lny;

  a0 = Min (a, b);
  if (a0 >= 8.)
    {
      goto L130;
    }

  if (x > .375)
    {
      goto L10;
    }
  lnx = log (x);
  d__1 = -(x);
  lny = cdf_dln1px (d__1);
  goto L30;
 L10:
  if (y > .375)
    {
      goto L20;
    }
  d__1 = -(y);
  lnx = cdf_dln1px (d__1);
  lny = log (y);
  goto L30;
 L20:
  lnx = log (x);
  lny = log (y);

 L30:
  z = a * lnx + b * lny;
  if (a0 < 1.)
    {
      goto L40;
    }
  z -= cdf_betaln (a, b);
  ret_val = cdf_esum (mu, z);
  return ret_val;
 L40:
  /*              PROCEDURE FOR A .LT. 1 OR B .LT. 1 */
  b0 = Max (a, b);
  if (b0 >= 8.)
    {
      goto L120;
    }
  if (b0 > 1.)
    {
      goto L70;
    }
  /*                   ALGORITHM FOR B0 .LE. 1 */
  ret_val = cdf_esum (mu, z);
  if (ret_val == 0.)
    {
      return ret_val;
    }

  apb = a + b;
  if (apb > 1.)
    {
      goto L50;
    }
  z = cdf_gam1 (apb) + 1.;
  goto L60;
 L50:
  u = a + b - 1.;
  z = (cdf_gam1 (u) + 1.) / apb;

 L60:
  c__ = (cdf_gam1 (a) + 1.) * (cdf_gam1 (b) + 1.) / z;
  ret_val = ret_val * (a0 * c__) / (a0 / b0 + 1.);
  return ret_val;


 L70:
  /*                ALGORITHM FOR 1 .LT. B0 .LT. 8 */
  u = cdf_gamln1 (a0);
  n = (int) (b0 - 1.);
  if (n < 1)
    {
      goto L90;
    }
  c__ = 1.;
  i1 = n;
  for (i = 1; i <= i1; ++i)
    {
      b0 += -1.;
      c__ *= b0 / (a0 + b0);
      /* L80: */
    }
  u = log (c__) + u;

 L90:
  z -= u;
  b0 += -1.;
  apb = a0 + b0;
  if (apb > 1.)
    {
      goto L100;
    }
  t = cdf_gam1 (apb) + 1.;
  goto L110;
 L100:
  u = a0 + b0 - 1.;
  t = (cdf_gam1 (u) + 1.) / apb;
 L110:
  ret_val = a0 * cdf_esum (mu, z) * (cdf_gam1 (b0) + 1.) / t;
  return ret_val;

 L120:
  /*                   ALGORITHM FOR B0 .GE. 8 */
  u = cdf_gamln1 (a0) + cdf_algdiv (a0, b0);
  d__1 = z - u;
  ret_val = a0 * cdf_esum (mu, d__1);
  return ret_val;
 L130:
  /*              PROCEDURE FOR A .GE. 8 AND B .GE. 8 */
  if (a > b)
    {
      goto L140;
    }
  h__ = a / b;
  x0 = h__ / (h__ + 1.);
  y0 = 1. / (h__ + 1.);
  lambda = a - (a + b) * x;
  goto L150;
 L140:
  h__ = b / a;
  x0 = 1. / (h__ + 1.);
  y0 = h__ / (h__ + 1.);
  lambda = (a + b) * y - b;
 L150:
  e = -lambda / a;
  if (Abs (e) > .6)
    {
      goto L160;
    }
  u = cdf_rlog1 (e);
  goto L170;
 L160:
  u = e - log (x / x0);
 L170:
  e = lambda / b;
  if (Abs (e) > .6)
    {
      goto L180;
    }
  v = cdf_rlog1 (e);
  goto L190;
 L180:
  v = e - log (y / y0);
 L190:
  d__1 = -(a * u + b * v);
  z = cdf_esum (mu, d__1);
  ret_val = one_sqrttpi * sqrt (b * x0) * z * exp (-cdf_bcorr (a, b));
  return ret_val;
}	


/*
 *
 * EVALUATION OF EXP(MU + X) 
 */

static double cdf_esum (int mu, double x)
{
  double w;

  if (x > 0.)
    {
      if (mu > 0) return exp ((double) mu) * exp (x); 
      w = mu + x;
      if (w < 0.) return exp ((double) mu) * exp (x);
      return exp(w);
    }
  /* now x is <= 0 */
  if (mu < 0)  return exp ((double) mu) * exp (x);
  w = mu + x;
  if (w > 0.) return exp ((double) mu) * exp (x);
  return exp (w);
}

