#include "cdf.h"


/* ********************************************************************** */

/*      DOUBLE PRECISION FUNCTION DLNBET( A, B ) */
/*          Double precision LN of the complete BETa */


/*                              Function */


/*     Returns the natural log of the complete beta function, */
/*     i.e., */

/*                  ln( Gamma(a)*Gamma(b) / Gamma(a+b) */


/*                              Arguments */


/*   A,B --> The (symmetric) arguments to the complete beta */
/*                  DOUBLE PRECISION A, B */


/*                              Method */


/*     Renames BETALN from: */
/*     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant */
/*     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM */
/*     Trans. Math.  Softw. 18 (1993), 360-373. */

/* ********************************************************************** */

/* ----------------------------------------------------------------------- */
/*     EVALUATION OF THE LOGARITHM OF THE BETA FUNCTION */
/* ----------------------------------------------------------------------- */

double cdf_dlnbet (double *a0, double *b0)
{
  /*     E = 0.5*LN(2*PI) */
  const double e = .918938533204673;
  int i__1;
  double ret_val, d__1;
  double a, b, c__, h__;
  int i__, n;
  double u, v, w, z__;

  a = Min (*a0, *b0);
  b = Max (*a0, *b0);
  if (a >= 8.)
    {
      goto L100;
    }
  if (a >= 1.)
    {
      goto L20;
    }
/* ----------------------------------------------------------------------- */
/*                   PROCEDURE WHEN A .LT. 1 */
/* ----------------------------------------------------------------------- */
  if (b >= 8.)
    {
      goto L10;
    }
  d__1 = a + b;
  ret_val = cdf_gamln (&a) + (cdf_gamln (&b) - cdf_gamln (&d__1));
  return ret_val;
L10:
  ret_val = cdf_gamln (&a) + cdf_algdiv (&a, &b);
  return ret_val;
/* ----------------------------------------------------------------------- */
/*                PROCEDURE WHEN 1 .LE. A .LT. 8 */
/* ----------------------------------------------------------------------- */
L20:
  if (a > 2.)
    {
      goto L40;
    }
  if (b > 2.)
    {
      goto L30;
    }
  ret_val = cdf_gamln (&a) + cdf_gamln (&b) - cdf_gsumln (&a, &b);
  return ret_val;
L30:
  w = 0.;
  if (b < 8.)
    {
      goto L60;
    }
  ret_val = cdf_gamln (&a) + cdf_algdiv (&a, &b);
  return ret_val;

/*                REDUCTION OF A WHEN B .LE. 1000 */

L40:
  if (b > 1e3)
    {
      goto L80;
    }
  n = (int) (a - 1.);
  w = 1.;
  i__1 = n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      a += -1.;
      h__ = a / b;
      w *= h__ / (h__ + 1.);
/* L50: */
    }
  w = log (w);
  if (b < 8.)
    {
      goto L60;
    }
  ret_val = w + cdf_gamln (&a) + cdf_algdiv (&a, &b);
  return ret_val;

/*                 REDUCTION OF B WHEN B .LT. 8 */

L60:
  n = (int) (b - 1.);
  z__ = 1.;
  i__1 = n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      b += -1.;
      z__ *= b / (a + b);
/* L70: */
    }
  ret_val =
    w + log (z__) + (cdf_gamln (&a) + (cdf_gamln (&b) - cdf_gsumln (&a, &b)));
  return ret_val;

/*                REDUCTION OF A WHEN B .GT. 1000 */

L80:
  n = (int) (a - 1.);
  w = 1.;
  i__1 = n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      a += -1.;
      w *= a / (a / b + 1.);
/* L90: */
    }
  ret_val = log (w) - n * log (b) + (cdf_gamln (&a) + cdf_algdiv (&a, &b));
  return ret_val;
/* ----------------------------------------------------------------------- */
/*                   PROCEDURE WHEN A .GE. 8 */
/* ----------------------------------------------------------------------- */
L100:
  w = cdf_bcorr (&a, &b);
  h__ = a / b;
  c__ = h__ / (h__ + 1.);
  u = -(a - .5) * log (c__);
  v = b * cdf_alnrel (&h__);
  if (u <= v)
    {
      goto L110;
    }
  ret_val = log (b) * -.5 + e + w - v - u;
  return ret_val;
L110:
  ret_val = log (b) * -.5 + e + w - u - v;
  return ret_val;
}				/* dlnbet_ */
