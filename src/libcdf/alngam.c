#include "cdf.h"

/*
 *     double precision function alngam(x) 
 *                 double precision ln of the gamma function 
 *                              function 
 *     returns the natural logarithm of gamma(x). 
 *                              arguments 
 *     x --> value at which scaled log gamma is to be returned 
 *                    x is double precision 
 *                              method 
 *     if x .le. 6.0, then use recursion to get x below 3 
 *     then apply rational approximation number 5236 of 
 *     hart et al, computer approximations, John Wiley and sons, ny, 1968. 
 *     if x .gt. 6.0, then use recursion to get x to at least 12 and 
 *     then use formula 5423 of the same source. 
 */

double cdf_alngam (double *x)
{
  const int c__9 = 9;
  const int c__4 = 4;
  const int c__5 = 5;

  const double hln2pi=0.91893853320467274178E0;
  const double scoefn[9] =
    { 62.003838007127258804, 36.036772530024836321, 20.782472531792126786,
      6.338067999387272343, 2.15994312846059073, .3980671310203570498, .1093115956710439502,
      .0092381945590275995, .0029737866448101651 };
  const double scoefd[4] =
    { 62.003838007126989331, 9.822521104713994894, -8.906016659497461257,1. };
  const double coef[5] =
    { .083333333333333023564, -.0027777777768818808, 7.9365006754279e-4,
      -5.94997310889e-4, 8.065880899e-4 };

  int i__1;
  double ret_val, d__1, d__2;
  double prod;
  int i__, n;
  double xx, offset;

  if (!(*x <= 6.))
    {
      goto L70;
    }
  prod = 1.;
  xx = *x;
  if (!(*x > 3.))
    {
      goto L30;
    }
 L10:
  if (!(xx > 3.))
    {
      goto L20;
    }
  xx += -1.;
  prod *= xx;
  goto L10;
 L20:
 L30:
  if (!(*x < 2.))
    {
      goto L60;
    }
 L40:
  if (!(xx < 2.))
    {
      goto L50;
    }
  prod /= xx;
  xx += 1.;
  goto L40;
 L50:
 L60:
  d__1 = xx - 2.;
  d__2 = xx - 2.;
  ret_val =  cdf_devlpl (scoefn, &c__9, &d__1) / cdf_devlpl (scoefd, &c__4, &d__2);
  /*     COMPUTE RATIONAL APPROXIMATION TO GAMMA(X) */
  ret_val *= prod;
  ret_val = log (ret_val);
  goto L110;
 L70:
  offset = hln2pi;
  /*     IF NECESSARY MAKE X AT LEAST 12 AND CARRY CORRECTION IN OFFSET */
  /*     ADDED TO PREVENT INT OVERFLOW IN int(12.0E0-x) S. STEER */
  if (*x > 12.)
    {
      goto L90;
    }

  n = (int) (12. - *x);
  if (!(n > 0))
    {
      goto L90;
    }
  prod = 1.;
  i__1 = n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      prod *= *x + (double) (i__ - 1);
      /* L80: */
    }
  offset -= log (prod);
  xx = *x + (double) n;
  goto L100;
 L90:
  xx = *x;
  /*     COMPUTE POWER SERIES */
 L100:
  /* Computing 2nd power */
  d__2 = xx;
  d__1 = 1. / (d__2 * d__2);
  ret_val = cdf_devlpl (coef, &c__5, &d__1) / xx;
  ret_val = ret_val + offset + (xx - .5) * log (xx) - xx;
 L110:
  return ret_val;
} 


