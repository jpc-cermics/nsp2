#include "cdf.h"

/* ----------------------------------------------------------------------- */
/*            EVALUATION OF THE FUNCTION EXP(X) - 1 */
/* ----------------------------------------------------------------------- */

double cdf_rexp (double *x)
{
  const double p1 = 9.14041914819518e-10;
  const double p2 = .0238082361044469;
  const double q1 = -.499999999085958;
  const double q2 = .107141568980644;
  const double q3 = -.0119041179760821;
  const double q4 = 5.95130811860248e-4;
  double ret_val;
  double w;
  if (Abs (*x) > .15)
    {
      goto L10;
    }
  ret_val =
    *x * (((p2 * *x + p1) * *x + 1.) /
	  ((((q4 * *x + q3) * *x + q2) * *x + q1) * *x + 1.));
  return ret_val;

L10:
  w = exp (*x);
  if (*x > 0.)
    {
      goto L20;
    }
  ret_val = w - .5 - .5;
  return ret_val;
L20:
  ret_val = w * (.5 - 1. / w + .5);
  return ret_val;
}				/* rexp_ */
