#include "cdf.h"

/**
 * cdf_gamma:
 * @a: 
 * 
 * evaluation of the gamma function for real arguments 
 * gamma(a) is assigned the value 0 when the gamma function cannot
 * be computed.
 * written by alfred h. morris, jr. naval surface weapons center
 * dahlgren, virginia 
 * 
 * Returns: a double 
 **/

double cdf_gamma (double a)
{
  /*     D = 0.5*(LN(2*PI) - 1) */
  const int c__3 = 3;
  const int c__0 = 0;
  const double pi = 3.1415926535898;
  const double d__ = .41893853320467274178;
  const double p[7] =  { 5.39637273585445e-4, .0026193926004269, .020449366759492,
			 .0730981088720487, .279648642639792, .553413866010467, 1. };
  const double q[7] =  { -8.32979206704073e-4, .00470059485860584, .022521113103534,
			 -.17045896931336, -.056790276197494, 1.13062953091122, 1. };
  const double r1 = 8.20756370353826e-4;
  const double r2 = -5.95156336428591e-4;
  const double r3 = 7.93650663183693e-4;
  const double r4 = -.00277777777770481;
  const double r5 = .0833333333333333;

  double bot, lnx, top;
  double g;
  double ret_val;
  double sx=0,t, w, x, z__;
  int i__, j, m, n;
  int i__1;

  ret_val = 0.;
  x = a;
  if (Abs (a) >= 15.)
    {
      goto L110;
    }
  /*
   *            EVALUATION OF GAMMA(A) FOR ABS(A) .LT. 15 
   */

  t = 1.;
  m = (int) (a) - 1;
  /*     LET T BE THE PRODUCT OF A-J WHEN A .GE. 2 */
  if (m < 0)
    {
      goto L40;
    }
  else if (m == 0)
    {
      goto L30;
    }
  else
    {
      goto L10;
    }
L10:
  i__1 = m;
  for (j = 1; j <= i__1; ++j)
    {
      x += -1.;
      t = x * t;
    }
L30:
  x += -1.;
  goto L80;
  /*     LET T BE THE PRODUCT OF A+J WHEN A .LT. 1 */
 L40:
  t = a;
  if (a > 0.)
    {
      goto L70;
    }
  m = -m - 1;
  if (m == 0)
    {
      goto L60;
    }
  i__1 = m;
  for (j = 1; j <= i__1; ++j)
    {
      x += 1.;
      t = x * t;
      /* L50: */
    }
L60:
  x = x + .5 + .5;
  t = x * t;
  if (t == 0.)
    {
      return ret_val;
    }
 L70:
  /*     THE FOLLOWING CODE CHECKS IF 1/T CAN OVERFLOW. THIS  
   *     CODE MAY BE OMITTED IF DESIRED. 
   */
  if (Abs (t) >= 1e-30)
    {
      goto L80;
    }
  if (Abs (t) * cdf_spmpar (c__3) <= 1.0001)
    {
      return ret_val;
    }
  ret_val = 1. / t;
  return ret_val;
  /*     COMPUTE GAMMA(1 + X) FOR  0 .LE. X .LT. 1 */
 L80:
  top = p[0];
  bot = q[0];
  for (i__ = 2; i__ <= 7; ++i__)
    {
      top = p[i__ - 1] + x * top;
      bot = q[i__ - 1] + x * bot;
      /* L90: */
    }
  ret_val = top / bot;
  /*     TERMINATION */
  if (a < 1.)
    {
      goto L100;
    }
  ret_val *= t;
  return ret_val;
L100:
  ret_val /= t;
  return ret_val;
  /*
   *            EVALUATION OF GAMMA(A) FOR ABS(A) .GE. 15 
   */
 L110:
  if (Abs (a) >= 1e3)
    {
      return ret_val;
    }
  if (a > 0.)
    {
      goto L120;
    }
  x = -(a);
  n = (int) x;
  t = x - n;
  if (t > .9)
    {
      t = 1. - t;
    }
  sx = sin (pi * t) / pi;
  if (n % 2 == 0)
    {
      sx = -sx;
    }
  if (sx == 0.)
    {
      return ret_val;
    }
  /*     COMPUTE THE MODIFIED ASYMPTOTIC SUM */
 L120:
  t = 1. / (x * x);
  g = ((((r1 * t + r2) * t + r3) * t + r4) * t + r5) / x;
  /*    ONE MAY REPLACE THE NEXT STATEMENT WITH  LNX = ALOG(X)
   *     BUT LESS ACCURACY WILL NORMALLY BE OBTAINED. 
   */
  lnx = log (x);
  /*     FINAL ASSEMBLY */
  z__ = x;
  g = d__ + g + (z__ - .5) * (lnx - 1.);
  w = g;
  t = g - w;
  if (w > cdf_exparg (c__0) * .99999)
    {
      return ret_val;
    }
  ret_val = exp (w) * (t + 1.);
  if (a < 0.)
    {
      ret_val = 1. / (ret_val * sx) / x;
    }
  return ret_val;
}		


