#include "cdf.h"

/* ********************************************************************** */

/*     DOUBLE PRECISION FUNCTION DT1(P,Q,DF) */
/*     Double precision Initalize Approximation to */
/*           INVerse of the cumulative T distribution */


/*                              Function */


/*     Returns  the  inverse   of  the T   distribution   function, i.e., */
/*     the integral from 0 to INVT of the T density is P. This is an */
/*     initial approximation */


/*                              Arguments */


/*     P --> The p-value whose inverse from the T distribution is */
/*          desired. */
/*                    P is DOUBLE PRECISION */

/*     Q --> 1-P. */
/*                    Q is DOUBLE PRECISION */

/*     DF --> Degrees of freedom of the T distribution. */
/*                    DF is DOUBLE PRECISION */

/* ********************************************************************** */


double cdf_dt1 (double *p, double *q, double *df)
{
  const double coef[20] /* was [5][4] */  =
    { 1., 1., 0., 0., 0., 3., 16., 5., 0., 0., -15., 17., 19., 3., 0., -945.,
      -1920., 1482., 776., 79. };
  const  int ideg[4] = { 2, 3, 4, 5 };
  const double denom[4] = { 4., 96., 384., 92160. };
  double ret_val, d__1;
  double term;
  int i__;
  double x, xp, xx;
  double denpow;
  double sum;

  x = (d__1 = cdf_dinvnr (p, q), Abs (d__1));
  xx = x * x;
  sum = x;
  denpow = 1.;
  for (i__ = 1; i__ <= 4; ++i__)
    {
      term = cdf_devlpl (&coef[i__ * 5 - 5], ideg[i__ - 1], xx) * x;
      denpow *= *df;
      sum += term / (denpow * denom[i__ - 1]);
/* L10: */
    }
  if (!(*p >= .5))
    {
      goto L20;
    }
  xp = sum;
  goto L30;
L20:
  xp = -sum;
L30:
  ret_val = xp;
  return ret_val;
}				/* dt1_ */
