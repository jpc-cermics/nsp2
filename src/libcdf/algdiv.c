#include "cdf.h"

/* ----------------------------------------------------------------------- */
/*     COMPUTATION OF LN(GAMMA(B)/GAMMA(A+B)) WHEN B .GE. 8 */
/*                         -------- */
/*     IN THIS ALGORITHM, DEL(X) IS THE FUNCTION DEFINED BY */
/*     LN(GAMMA(X)) = (X - 0.5)*LN(X) - X + 0.5*LN(2*PI) + DEL(X). */
/* ----------------------------------------------------------------------- */

double cdf_algdiv (double *a, double *b)
{
  const double c0 = .0833333333333333;
  const double c1 = -.00277777777760991;
  const double c2 = 7.9365066682539e-4;
  const double c3 = -5.9520293135187e-4;
  const double c4 = 8.37308034031215e-4;
  const double c5 = -.00165322962780713;
  double ret_val, d__1;
  double c__, d__, h__, t, u, v, w, x, s3, s5, s7, x2, s9, s11;

  if (*a <= *b)
    {
      goto L10;
    }
  h__ = *b / *a;
  c__ = 1. / (h__ + 1.);
  x = h__ / (h__ + 1.);
  d__ = *a + (*b - .5);
  goto L20;
 L10:
  h__ = *a / *b;
  c__ = h__ / (h__ + 1.);
  x = 1. / (h__ + 1.);
  d__ = *b + (*a - .5);
  /*                SET SN = (1 - X**N)/(1 - X) */
 L20:
  x2 = x * x;
  s3 = x + x2 + 1.;
  s5 = x + x2 * s3 + 1.;
  s7 = x + x2 * s5 + 1.;
  s9 = x + x2 * s7 + 1.;
  s11 = x + x2 * s9 + 1.;
  /*                SET W = DEL(B) - DEL(A + B) */
  /* Computing 2nd power */
  d__1 = 1. / *b;
  t = d__1 * d__1;
  w =
    ((((c5 * s11 * t + c4 * s9) * t + c3 * s7) * t + c2 * s5) * t +
     c1 * s3) * t + c0;
  w *= c__ / *b;
  /*                    COMBINE THE RESULTS */
  d__1 = *a / *b;
  u = d__ * cdf_alnrel (&d__1);
  v = *a * (log (*b) - 1.);
  if (u <= v)
    {
      goto L30;
    }
  ret_val = w - v - u;
  return ret_val;
L30:
  ret_val = w - u - v;
  return ret_val;
}
