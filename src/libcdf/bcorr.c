#include "cdf.h"

/* ----------------------------------------------------------------------- */
/*     EVALUATION OF  DEL(A0) + DEL(B0) - DEL(A0 + B0)  WHERE */
/*     LN(GAMMA(A)) = (A - 0.5)*LN(A) - A + 0.5*LN(2*PI) + DEL(A). */
/*     IT IS ASSUMED THAT A0 .GE. 8 AND B0 .GE. 8. */
/* ----------------------------------------------------------------------- */

double cdf_bcorr (double *a0, double *b0)
{
  const double c0 = .0833333333333333;
  const double c1 = -.00277777777760991;
  const double c2 = 7.9365066682539e-4;
  const double c3 = -5.9520293135187e-4;
  const double c4 = 8.37308034031215e-4;
  const double c5 = -.00165322962780713;

  /* System generated locals */
  double ret_val, d__1;

  /* Local variables */
  double a, b, c__, h__, t, w, x, s3, s5, s7, x2, s9, s11;

  a = Min (*a0, *b0);
  b = Max (*a0, *b0);

  h__ = a / b;
  c__ = h__ / (h__ + 1.);
  x = 1. / (h__ + 1.);
  x2 = x * x;

  /*                SET SN = (1 - X**N)/(1 - X) */

  s3 = x + x2 + 1.;
  s5 = x + x2 * s3 + 1.;
  s7 = x + x2 * s5 + 1.;
  s9 = x + x2 * s7 + 1.;
  s11 = x + x2 * s9 + 1.;

  /*                SET W = DEL(B) - DEL(A + B) */

  /* Computing 2nd power */
  d__1 = 1. / b;
  t = d__1 * d__1;
  w =
    ((((c5 * s11 * t + c4 * s9) * t + c3 * s7) * t + c2 * s5) * t +
     c1 * s3) * t + c0;
  w *= c__ / b;

/*                   COMPUTE  DEL(A) + W */

/* Computing 2nd power */
  d__1 = 1. / a;
  t = d__1 * d__1;
  ret_val = (((((c5 * t + c4) * t + c3) * t + c2) * t + c1) * t + c0) / a + w;
  return ret_val;
}				/* bcorr_ */
