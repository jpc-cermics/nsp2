#include "cdf.h"

/* Table of constant values */

const double c_b5 = 1.;

/*
 *     cdf_erf function returns the error function of x; defined as
 *        erf(x) = 2/sqrt(pi)* integral from 0 to x of exp(-t*t) dt
 *     cdf_erfc(0,x) function returns the complementary error function of  x,
 *       that is 1.0 - erf(x).
 *     cdf_erfc(1,x) function returns 
 *        y = exp(x^2 ) * erfc(x)
 *                   1
 *        y  -->  ---------    when x --> +oo
 *                x sqrt(pi) 
 *
 *     
 *
 */

double cdf_erf (double *x)
{
  const double c__ = .564189583547756;
  const double a[5] =
    { 7.7105849500132e-5, -.00133733772997339, .0323076579225834,
      .0479137145607681, .128379167095513 };
  const double b[3] =
    { .00301048631703895, .0538971687740286, .375795757275549 };
  const double p[8] =
    { -1.36864857382717e-7, .564195517478974, 7.21175825088309,
      43.1622272220567, 152.98928504694, 339.320816734344, 451.918953711873, 300.459261020162 };
  const double q[8] =
    { 1., 12.7827273196294, 77.0001529352295, 277.585444743988,
      638.980264465631, 931.35409485061, 790.950925327898, 300.459260956983 };
  const double r__[5] =
    { 2.10144126479064, 26.2370141675169, 21.3688200555087, 4.6580782871847,
      .282094791773523 };
  const double s[4] =
    { 94.153775055546, 187.11481179959, 99.0191814623914, 18.0124575948747 };

  double ret_val;
  double t, x2, ax, bot, top;

  ax = Abs (*x);
  if (ax > .5)
    {
      goto L10;
    }
  t = *x * *x;
  top = (((a[0] * t + a[1]) * t + a[2]) * t + a[3]) * t + a[4] + 1.;
  bot = ((b[0] * t + b[1]) * t + b[2]) * t + 1.;
  ret_val = *x * (top / bot);
  return ret_val;

L10:
  if (ax > 4.)
    {
      goto L20;
    }
  top =
    ((((((p[0] * ax + p[1]) * ax + p[2]) * ax + p[3]) * ax + p[4]) * ax +
      p[5]) * ax + p[6]) * ax + p[7];
  bot =
    ((((((q[0] * ax + q[1]) * ax + q[2]) * ax + q[3]) * ax + q[4]) * ax +
      q[5]) * ax + q[6]) * ax + q[7];
  ret_val = .5 - exp (-(*x) * *x) * top / bot + .5;
  if (*x < 0.)
    {
      ret_val = -ret_val;
    }
  return ret_val;

L20:
  if (ax >= 5.8)
    {
      goto L30;
    }
  x2 = *x * *x;
  t = 1. / x2;
  top = (((r__[0] * t + r__[1]) * t + r__[2]) * t + r__[3]) * t + r__[4];
  bot = (((s[0] * t + s[1]) * t + s[2]) * t + s[3]) * t + 1.;
  ret_val = (c__ - top / (x2 * bot)) / ax;
  ret_val = .5 - exp (-x2) * ret_val + .5;
  if (*x < 0.)
    {
      ret_val = -ret_val;
    }
  return ret_val;

L30:
  ret_val = D_SIGN (c_b5, *x);
  return ret_val;
}

/*
 * complementary error function 
 */


double cdf_erfc (int ind, double *x)
{
  const int c__1 = 1;
  const double c__ = .564189583547756;
  const double a[5] =
    { 7.7105849500132e-5, -.00133733772997339, .0323076579225834,
    .0479137145607681, .128379167095513
  };
  const double b[3] =
    { .00301048631703895, .0538971687740286, .375795757275549 };
  const double p[8] =
    { -1.36864857382717e-7, .564195517478974, 7.21175825088309,
    43.1622272220567, 152.98928504694, 339.320816734344, 451.918953711873,
      300.459261020162
  };
  const double q[8] =
    { 1., 12.7827273196294, 77.0001529352295, 277.585444743988,
    638.980264465631, 931.35409485061, 790.950925327898, 300.459260956983
  };
  const double r__[5] =
    { 2.10144126479064, 26.2370141675169, 21.3688200555087, 4.6580782871847,
    .282094791773523
  };
  const double s[4] =
    { 94.153775055546, 187.11481179959, 99.0191814623914, 18.0124575948747 };
  double ret_val, d__1;
  double e, t, w, ax;
  double bot, top;

  /*                     ABS(X) .LE. 0.5 */

  ax = Abs (*x);
  if (ax > .5)
    {
      goto L10;
    }
  t = *x * *x;
  top = (((a[0] * t + a[1]) * t + a[2]) * t + a[3]) * t + a[4] + 1.;
  bot = ((b[0] * t + b[1]) * t + b[2]) * t + 1.;
  ret_val = .5 - *x * (top / bot) + .5;
  if (ind != 0)
    {
      ret_val = exp (t) * ret_val;
    }
  return ret_val;

/*                  0.5 .LT. ABS(X) .LE. 4 */

L10:
  if (ax > 4.)
    {
      goto L20;
    }
  top =
    ((((((p[0] * ax + p[1]) * ax + p[2]) * ax + p[3]) * ax + p[4]) * ax +
      p[5]) * ax + p[6]) * ax + p[7];
  bot =
    ((((((q[0] * ax + q[1]) * ax + q[2]) * ax + q[3]) * ax + q[4]) * ax +
      q[5]) * ax + q[6]) * ax + q[7];
  ret_val = top / bot;
  goto L40;

/*                      ABS(X) .GT. 4 */

L20:
  if (*x <= -5.6)
    {
      goto L60;
    }
  if (ind != 0)
    {
      goto L30;
    }
  if (*x > 100.)
    {
      goto L70;
    }
  if (*x * *x > -cdf_exparg (c__1))
    {
      goto L70;
    }

L30:
/* Computing 2nd power */
  d__1 = 1. / *x;
  t = d__1 * d__1;
  top = (((r__[0] * t + r__[1]) * t + r__[2]) * t + r__[3]) * t + r__[4];
  bot = (((s[0] * t + s[1]) * t + s[2]) * t + s[3]) * t + 1.;
  ret_val = (c__ - t * top / bot) / ax;

/*                      FINAL ASSEMBLY */

L40:
  if (ind == 0)
    {
      goto L50;
    }
  if (*x < 0.)
    {
      ret_val = exp (*x * *x) * 2. - ret_val;
    }
  return ret_val;
L50:
  w = *x * *x;
  t = w;
  e = w - t;
  ret_val = (.5 - e + .5) * exp (-t) * ret_val;
  if (*x < 0.)
    {
      ret_val = 2. - ret_val;
    }
  return ret_val;

/*             LIMIT VALUE FOR LARGE NEGATIVE X */

L60:
  ret_val = 2.;
  if (ind != 0)
    {
      ret_val = exp (*x * *x) * 2.;
    }
  return ret_val;

/*             LIMIT VALUE FOR LARGE POSITIVE X */
/*                       WHEN IND = 0 */

L70:
  ret_val = 0.;
  return ret_val;
}				/* erfc1_ */
