#include "cdf.h"

/* ----------------------------------------------------------------------- */
/*             EVALUATION OF THE FUNCTION X - LN(1 + X) */
/* ----------------------------------------------------------------------- */

double cdf_rlog1 (double *x)
{
  const double a = .0566749439387324;
  const double b = .0456512608815524;
  const double p0 = .333333333333333;
  const double p1 = -.224696413112536;
  const double p2 = .00620886815375787;
  const double q1 = -1.27408923933623;
  const double q2 = .354508718369557;
  double ret_val;
  double h__, r__, t, w, w1;

  if (*x < -.39 || *x > .57)
    {
      goto L40;
    }
  if (*x < -.18)
    {
      goto L10;
    }
  if (*x > .18)
    {
      goto L20;
    }
  /*              ARGUMENT REDUCTION */
  h__ = *x;
  w1 = 0.;
  goto L30;

L10:
  h__ = *x + .3;
  h__ /= .7;
  w1 = a - h__ * .3;
  goto L30;

L20:
  h__ = *x * .75 - .25;
  w1 = b + h__ / 3.;

/*               SERIES EXPANSION */

L30:
  r__ = h__ / (h__ + 2.);
  t = r__ * r__;
  w = ((p2 * t + p1) * t + p0) / ((q2 * t + q1) * t + 1.);
  ret_val = t * 2. * (1. / (1. - r__) - r__ * w) + w1;
  return ret_val;


L40:
  w = *x + .5 + .5;
  ret_val = *x - log (w);
  return ret_val;
}				/* rlog1_ */
