#include "cdf.h"

/*
 *     evaluation of the digamma function 
 *
 *     psi1(xx) is assigned the value 0 when the digamma function cannot 
 *     be computed. 
 *     the main computation involves evaluation of rational chebyshev 
 *     approximations published in math. comp. 27, 123-127(1973) by 
 *     cody, strecok and thacher. 
 *
 *     psi1 was written at argonne national laboratory for the funpack 
 *     package of special function subroutines. psi1 was modified by 
 *     a.h. Morris (nswc). 
 *
 */

double cdf_psi1 (double *xx)
{
  const int c__3 = 3;
  const int c__1 = 1;
  const double piov4 = .785398163397448;
  const double dx0 = 1.461632144968362341262659542325721325;
  const double p1[7] =
    { .0089538502298197, 4.77762828042627, 142.441585084029, 1186.45200713425,
      3633.51846806499, 4138.10161269013, 1305.60269827897 };
  const double q1[6] =
    { 44.8452573429826, 520.752771467162, 2210.0079924783, 3641.27349079381,
      1908.310765963, 6.91091682714533e-6 };
  const double p2[4] =
    { -2.12940445131011, -7.01677227766759, -4.48616543918019,
      -.648157123766197 };
  const double q2[4] =
    { 32.2703493791143, 89.2920700481861, 54.6117738103215,
      7.77788548522962 };

  /* System generated locals */
  double ret_val, d__1, d__2;
  /* Local variables */
  double xmax1;
  int i__, m, n;
  double w, x, z__, upper;
  int nq;
  double xsmall;
  double den, aug, sgn, xmx0;
  /* --------------------------------------------------------------------- */
  /*     PIOV4 = PI/4 */
  /*     DX0 = ZERO OF PSI1 TO EXTENDED PRECISION */
  /* --------------------------------------------------------------------- */
  /* --------------------------------------------------------------------- */
  /*     COEFFICIENTS FOR RATIONAL APPROXIMATION OF */
  /*     PSI1(X) / (X - X0),  0.5 .LE. X .LE. 3.0 */
  /* --------------------------------------------------------------------- */
  /* --------------------------------------------------------------------- */
  /*     COEFFICIENTS FOR RATIONAL APPROXIMATION OF */
  /*     PSI1(X) - LN(X) + 1 / (2*X),  X .GT. 3.0 */
  /*     MACHINE DEPENDENT CONSTANTS ... */
  /*        XMAX1  = THE SMALLEST POSITIVE FLOATING POINT CONSTANT */
  /*                 WITH ENTIRELY INT REPRESENTATION.  ALSO USED */
  /*                 AS NEGATIVE OF LOWER BOUND ON ACCEPTABLE NEGATIVE */
  /*                 ARGUMENTS AND AS THE POSITIVE ARGUMENT BEYOND WHICH */
  /*                 PSI1 MAY BE REPRESENTED AS ALOG(X). */
  /*        XSMALL = ABSOLUTE ARGUMENT BELOW WHICH PI*COTAN(PI*X) */
  /*                 MAY BE REPRESENTED BY 1/X. */
  /* --------------------------------------------------------------------- */
  xmax1 = (double) cdf_ipmpar (&c__3);
  /* Computing MIN */
  d__1 = xmax1, d__2 = 1. / cdf_spmpar (c__1);
  xmax1 = Min (d__1, d__2);
  xsmall = 1e-9;
  /* --------------------------------------------------------------------- */
  x = *xx;
  aug = 0.;
  if (x >= .5)
    {
      goto L50;
    }
  /* --------------------------------------------------------------------- */
  /*     X .LT. 0.5,  USE REFLECTION FORMULA */
  /*     PSI1(1-X) = PSI1(X) + PI * COTAN(PI*X) */
  /* --------------------------------------------------------------------- */
  if (Abs (x) > xsmall)
    {
      goto L10;
    }
  if (x == 0.)
    {
      goto L100;
    }
  /* --------------------------------------------------------------------- */
  /*     0 .LT. ABS(X) .LE. XSMALL.  USE 1/X AS A SUBSTITUTE */
  /*     FOR  PI*COTAN(PI*X) */
  /* --------------------------------------------------------------------- */
  aug = -1. / x;
  goto L40;
  /* --------------------------------------------------------------------- */
  /*     REDUCTION OF ARGUMENT FOR COTAN */
  /* --------------------------------------------------------------------- */
 L10:
  w = -x;
  sgn = piov4;
  if (w > 0.)
    {
      goto L20;
    }
  w = -w;
  sgn = -sgn;
  /* --------------------------------------------------------------------- */
  /*     MAKE AN ERROR EXIT IF X .LE. -XMAX1 */
  /* --------------------------------------------------------------------- */
 L20:
  if (w >= xmax1)
    {
      goto L100;
    }
  nq = (int) w;
  w -= (double) nq;
  nq = (int) (w * 4.);
  w = (w - (double) nq * .25) * 4.;
  /* --------------------------------------------------------------------- */
  /*     W IS NOW RELATED TO THE FRACTIONAL PART OF  4.0 * X. */
  /*     ADJUST ARGUMENT TO CORRESPOND TO VALUES IN FIRST */
  /*     QUADRANT AND DETERMINE SIGN */
  /* --------------------------------------------------------------------- */
  n = nq / 2;
  if (n + n != nq)
    {
      w = 1. - w;
    }
  z__ = piov4 * w;
  m = n / 2;
  if (m + m != n)
    {
      sgn = -sgn;
    }
  /* --------------------------------------------------------------------- */
  /*     DETERMINE FINAL VALUE FOR  -PI*COTAN(PI*X) */
  /* --------------------------------------------------------------------- */
  n = (nq + 1) / 2;
  m = n / 2;
  m += m;
  if (m != n)
    {
      goto L30;
    }
  /* --------------------------------------------------------------------- */
  /*     CHECK FOR SINGULARITY */
  /* --------------------------------------------------------------------- */
  if (z__ == 0.)
    {
      goto L100;
    }
  /* --------------------------------------------------------------------- */
  /*     USE COS/SIN AS A SUBSTITUTE FOR COTAN, AND */
  /*     SIN/COS AS A SUBSTITUTE FOR TAN */
  /* --------------------------------------------------------------------- */
  aug = sgn * (cos (z__) / sin (z__) * 4.);
  goto L40;
 L30:
  aug = sgn * (sin (z__) / cos (z__) * 4.);
 L40:
  x = 1. - x;
 L50:
  if (x > 3.)
    {
      goto L70;
    }
  /* --------------------------------------------------------------------- */
  /*     0.5 .LE. X .LE. 3.0 */
  /* --------------------------------------------------------------------- */
  den = x;
  upper = p1[0] * x;

  for (i__ = 1; i__ <= 5; ++i__)
    {
      den = (den + q1[i__ - 1]) * x;
      upper = (upper + p1[i__]) * x;
      /* L60: */
    }

  den = (upper + p1[6]) / (den + q1[5]);
  xmx0 = x - dx0;
  ret_val = den * xmx0 + aug;
  return ret_val;
  /* --------------------------------------------------------------------- */
  /*     IF X .GE. XMAX1, PSI1 = LN(X) */
  /* --------------------------------------------------------------------- */
 L70:
  if (x >= xmax1)
    {
      goto L90;
    }
  /* --------------------------------------------------------------------- */
  /*     3.0 .LT. X .LT. XMAX1 */
  /* --------------------------------------------------------------------- */
  w = 1. / (x * x);
  den = w;
  upper = p2[0] * w;

  for (i__ = 1; i__ <= 3; ++i__)
    {
      den = (den + q2[i__ - 1]) * w;
      upper = (upper + p2[i__]) * w;
      /* L80: */
    }

  aug = upper / (den + q2[3]) - .5 / x + aug;
 L90:
  ret_val = aug + log (x);
  return ret_val;
  /* --------------------------------------------------------------------- */
  /*     ERROR RETURN */
  /* --------------------------------------------------------------------- */
 L100:
  ret_val = 0.;
  return ret_val;
} 

