#include "cdf.h"

/* ----------------------------------------------------------------------- */
/*          EVALUATION OF  EXP(MU) * (X**A*Y**B/BETA(A,B)) */
/* ----------------------------------------------------------------------- */

static double cdf_esum (int mu,double x);

double cdf_brcmp1 (int *mu, double *a, double *b, double *x, double *y)
{
  /*     CONST = 1/SQRT(2*PI) */
  const double const__ = .398942280401433;
  int i__1;
  double ret_val, d__1;
  double c__, e, h__;
  int i__, n;
  double t, u, v, z__;
  double a0, b0, x0, y0;
  double lambda;
  double apb, lnx, lny;

  a0 = Min (*a, *b);
  if (a0 >= 8.)
    {
      goto L130;
    }

  if (*x > .375)
    {
      goto L10;
    }
  lnx = log (*x);
  d__1 = -(*x);
  lny = cdf_alnrel (&d__1);
  goto L30;
L10:
  if (*y > .375)
    {
      goto L20;
    }
  d__1 = -(*y);
  lnx = cdf_alnrel (&d__1);
  lny = log (*y);
  goto L30;
L20:
  lnx = log (*x);
  lny = log (*y);

L30:
  z__ = *a * lnx + *b * lny;
  if (a0 < 1.)
    {
      goto L40;
    }
  z__ -= cdf_betaln (a, b);
  ret_val = cdf_esum (*mu, z__);
  return ret_val;
/* ----------------------------------------------------------------------- */
/*              PROCEDURE FOR A .LT. 1 OR B .LT. 1 */
/* ----------------------------------------------------------------------- */
L40:
  b0 = Max (*a, *b);
  if (b0 >= 8.)
    {
      goto L120;
    }
  if (b0 > 1.)
    {
      goto L70;
    }

/*                   ALGORITHM FOR B0 .LE. 1 */

  ret_val = cdf_esum (*mu, z__);
  if (ret_val == 0.)
    {
      return ret_val;
    }

  apb = *a + *b;
  if (apb > 1.)
    {
      goto L50;
    }
  z__ = cdf_gam1 (&apb) + 1.;
  goto L60;
L50:
  u = *a + *b - 1.;
  z__ = (cdf_gam1 (&u) + 1.) / apb;

L60:
  c__ = (cdf_gam1 (a) + 1.) * (cdf_gam1 (b) + 1.) / z__;
  ret_val = ret_val * (a0 * c__) / (a0 / b0 + 1.);
  return ret_val;

/*                ALGORITHM FOR 1 .LT. B0 .LT. 8 */

L70:
  u = cdf_gamln1 (&a0);
  n = (int) (b0 - 1.);
  if (n < 1)
    {
      goto L90;
    }
  c__ = 1.;
  i__1 = n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      b0 += -1.;
      c__ *= b0 / (a0 + b0);
/* L80: */
    }
  u = log (c__) + u;

L90:
  z__ -= u;
  b0 += -1.;
  apb = a0 + b0;
  if (apb > 1.)
    {
      goto L100;
    }
  t = cdf_gam1 (&apb) + 1.;
  goto L110;
L100:
  u = a0 + b0 - 1.;
  t = (cdf_gam1 (&u) + 1.) / apb;
L110:
  ret_val = a0 * cdf_esum (*mu, z__) * (cdf_gam1 (&b0) + 1.) / t;
  return ret_val;

/*                   ALGORITHM FOR B0 .GE. 8 */

L120:
  u = cdf_gamln1 (&a0) + cdf_algdiv (&a0, &b0);
  d__1 = z__ - u;
  ret_val = a0 * cdf_esum (*mu, d__1);
  return ret_val;
/* ----------------------------------------------------------------------- */
/*              PROCEDURE FOR A .GE. 8 AND B .GE. 8 */
/* ----------------------------------------------------------------------- */
L130:
  if (*a > *b)
    {
      goto L140;
    }
  h__ = *a / *b;
  x0 = h__ / (h__ + 1.);
  y0 = 1. / (h__ + 1.);
  lambda = *a - (*a + *b) * *x;
  goto L150;
L140:
  h__ = *b / *a;
  x0 = 1. / (h__ + 1.);
  y0 = h__ / (h__ + 1.);
  lambda = (*a + *b) * *y - *b;

L150:
  e = -lambda / *a;
  if (Abs (e) > .6)
    {
      goto L160;
    }
  u = cdf_rlog1 (&e);
  goto L170;
L160:
  u = e - log (*x / x0);

L170:
  e = lambda / *b;
  if (Abs (e) > .6)
    {
      goto L180;
    }
  v = cdf_rlog1 (&e);
  goto L190;
L180:
  v = e - log (*y / y0);

L190:
  d__1 = -(*a * u + *b * v);
  z__ = cdf_esum (*mu, d__1);
  ret_val = const__ * sqrt (*b * x0) * z__ * exp (-cdf_bcorr (a, b));
  return ret_val;
}				/* brcmp1_ */


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

