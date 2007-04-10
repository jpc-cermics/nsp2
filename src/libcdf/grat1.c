#include "cdf.h"



/**
 * cdf_grat1:
 * @a: 
 * @x: 
 * @r: 
 * @p: 
 * @q: 
 * @eps: 
 * 
 * evaluation of the incomplete gamma ratio functions 
 * p(a,x) and q(a,x) 
 * it is assumed that a <= 1. 
 * eps is the tolerance to be used. 
 * the input argument r has the value e**(-x)*x**a/gamma(a). 
 * 
 * 
 * Returns: an integer.
 **/

int cdf_grat1 (double *a, double *x, double *r, double *p, double *q, double *eps)
{
  const int c0 = 0;
  double d1;
  double a2nm1, b2nm1;
  double c, g, h, j, l, t, w, z, an, am0, an0, a2n, b2n, cma;
  double tol, sum;

  if (*a * *x == 0.)
    {
      if (*x <= *a)
	{
	  *p = 0.;
	  *q = 1.;
	  return 0;
	}
      else
	{
	  *p = 1.;
	  *q = 0.;
	  return 0;
	}
    }
  if (*a == .5)
    {
      if (*x >= .25)
	{
	  d1 = sqrt (*x);
	  *q = cdf_erfc (c0, d1);
	  *p = .5 - *q + .5;
	  return 0;
	}
      else 
	{
	  d1 = sqrt (*x);
	  *p = cdf_erf (d1);
	  *q = .5 - *p + .5;
	  return 0;
	}
    }

  if (*x >= 1.1)
    {
      /*              CONTINUED FRACTION EXPANSION */
      a2nm1 = 1.;
      a2n = 1.;
      b2nm1 = *x;
      b2n = *x + (1. - *a);
      c = 1.;
      while (1) 
	{
	  a2nm1 = *x * a2n + c * a2nm1;
	  b2nm1 = *x * b2n + c * b2nm1;
	  am0 = a2nm1 / b2nm1;
	  c += 1.;
	  cma = c - *a;
	  a2n = a2nm1 + cma * a2n;
	  b2n = b2nm1 + cma * b2n;
	  an0 = a2n / b2n;
	  if (!((d1 = an0 - am0, Abs (d1)) >= *eps * an0)) break;
	}
      *q = *r * an0;
      *p = .5 - *q + .5;
      return 0;
    }
  /*             TAYLOR SERIES FOR P(A,X)/X**A */

  an = 3.;
  c = *x;
  sum = *x / (*a + 3.);
  tol = *eps * .1 / (*a + 1.);
  while (1) 
    {
      an += 1.;
      c = -c * (*x / an);
      t = c / (*a + an);
      sum += t;
      if (! (Abs (t) > tol)) break;
    }

  j = *a * *x * ((sum / 6. - .5 / (*a + 2.)) * *x + 1. / (*a + 1.));

  z = *a * log (*x);
  h = cdf_gam1 (*a);
  g = h + 1.;
  if (*x < .25)
    {
      goto L30;
    }
  if (*a < *x / 2.59)
    {
      goto L50;
    }
  goto L40;
 L30:
  if (z > -.13394)
    {
      goto L50;
    }
 L40:
  w = exp (z);
  *p = w * g * (.5 - j + .5);
  *q = .5 - *p + .5;
  return 0;
 L50:
  l = cdf_rexp (z);
  w = l + .5 + .5;
  *q = (w * j - l) * g - h;
  if (*q < 0.)
    {
      *p = 1.;
      *q = 0.;
      return 0;
    }
  *p = .5 - *q + .5;
  return 0;


}			
