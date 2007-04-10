#include "cdf.h"

/**
 * cdf_betaln:
 * @a0: 
 * @b0: 
 * 
 * evaluation of the logarithm of the beta function 
 * 
 * Returns: a double 
 **/

double cdf_betaln (double a0, double b0)
{
  /* e = 0.5*LN(2*PI) */
  const double e = .918938533204673;
  int i1, i, n;
  double ret_val, d1;
  double a, b, c, h;
  double u, v, w, z;

  a = Min (a0, b0);
  b = Max (a0, b0);
  if (a >= 8.)
    {
      goto L100;
    }
  if (a >= 1.)
    {
      goto L20;
    }
  /* PROCEDURE WHEN A .LT. 1   */
  if (b >= 8.)
    {
      ret_val = cdf_gamln (a) + cdf_algdiv (a, b);
      return ret_val;
    }
  d1 = a + b;
  ret_val = cdf_gamln (a) + (cdf_gamln (b) - cdf_gamln (d1));
  return ret_val;

  /* PROCEDURE WHEN 1 .LE. A .LT. 8 */
 L20:
  if (a > 2.)
    {
      goto L40;
    }
  if (b > 2.)
    {
      goto L30;
    }
  ret_val = cdf_gamln (a) + cdf_gamln (b) - cdf_gsumln (a, b);
  return ret_val;
 L30:
  w = 0.;
  if (b < 8.)
    {
      goto L60;
    }
  ret_val = cdf_gamln (a) + cdf_algdiv (a, b);
  return ret_val;

  /* REDUCTION OF A WHEN B .LE. 1000 */
 L40:
  if (b > 1e3)
    {
      goto L80;
    }
  n = (int) (a - 1.);
  w = 1.;
  i1 = n;
  for (i = 1; i <= i1; ++i)
    {
      a += -1.;
      h = a / b;
      w *= h / (h + 1.);
      /* L50: */
    }
  w = log (w);
  if (b < 8.)
    {
      goto L60;
    }
  ret_val = w + cdf_gamln (a) + cdf_algdiv (a, b);
  return ret_val;

 L60:
  /* REDUCTION OF B WHEN B .LT. 8 */

  n = (int) (b - 1.);
  z = 1.;
  i1 = n;
  for (i = 1; i <= i1; ++i)
    {
      b += -1.;
      z *= b / (a + b);
    }
  ret_val =   w + log (z) + (cdf_gamln (a) + (cdf_gamln (b) - cdf_gsumln (a, b)));
  return ret_val;

 L80:
  /* REDUCTION OF A WHEN B .GT. 1000 */
  n = (int) (a - 1.);
  w = 1.;
  i1 = n;
  for (i = 1; i <= i1; ++i)
    {
      a += -1.;
      w *= a / (a / b + 1.);
    }
  ret_val = log (w) - n * log (b) + (cdf_gamln (a) + cdf_algdiv (a, b));
  return ret_val;

 L100:
  /* PROCEDURE WHEN A .GE. 8 */
  w = cdf_bcorr (&a, &b);
  h = a / b;
  c = h / (h + 1.);
  u = -(a - .5) * log (c);
  v = b * cdf_alnrel (h);
  if (u <= v)
    {
      ret_val = log (b) * -.5 + e + w - u - v;
      return ret_val;
    }
  else 
    {
      ret_val = log (b) * -.5 + e + w - v - u;
      return ret_val;
    }

}

