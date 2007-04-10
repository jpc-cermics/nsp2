#include "cdf.h"

/**
 * cdf_brcomp:
 * @a: 
 * @b: 
 * @x: 
 * @y: 
 * 
 * evaluation of x^a*y^b/beta(a,b)
 * 
 * Returns: a double 
 **/

double cdf_brcomp (double a, double b, double x, double y)
{
  /*     CONST = 1/SQRT(2*PI) */
  const  double invsqrt2pi = .398942280401433;
  double a0, b0, x0, y0,  apb, lnx, lny;
  double c, e, h, lambda,  ret_val, d1,  t, u, v, z;
  int i, n,  i1;

  ret_val = 0.;
  if (x == 0. || y == 0.)
    {
      return ret_val;
    }
  a0 = Min (a, b);
  if (a0 >= 8.)
    {
      /*              PROCEDURE FOR A .GE. 8 AND B .GE. 8 */
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
      z = exp (-(a * u + b * v));
      ret_val = invsqrt2pi * sqrt (b * x0) * z * exp (-cdf_bcorr (&a, &b));
      return ret_val;
    }
  
  if (x > .375)
    {
      if (y > .375)
	{
	  lnx = log (x);
	  lny = log (y);
	}
      else
	{
	  d1 = -(y);
	  lnx = cdf_alnrel (d1);
	  lny = log (y);
	}
    }
  else 
    {
      lnx = log (x);
      d1 = -(x);
      lny = cdf_alnrel (d1);
    }
  z = a * lnx + b * lny;
  if (a0 >= 1.)
    {
      z -= cdf_betaln (a, b);
      ret_val = exp (z);
      return ret_val;
    }

  /* PROCEDURE FOR A .LT. 1 OR B .LT. 1 */
  b0 = Max (a, b);
  if (b0 >= 8.)
    {
      /*  ALGORITHM FOR B0 .GE. 8 */
      u = cdf_gamln1 (a0) + cdf_algdiv (a0, b0);
      ret_val = a0 * exp (z - u);
      return ret_val;
    }
  if (b0 > 1.)
    {
      /*                ALGORITHM FOR 1 .LT. B0 .LT. 8 */
      u = cdf_gamln1 (a0);
      n = (int) (b0 - 1.);
      if (n < 1)
	{
	}
      else 
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
      ret_val = a0 * exp (z) * (cdf_gam1 (b0) + 1.) / t;
      return ret_val;
    }

  /*  ALGORITHM FOR B0 .LE. 1 */
  ret_val = exp (z);
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

