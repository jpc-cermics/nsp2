#include "cdf.h"

/**
 * cdf_alngam:
 * @x: pointer to double 
 * 
 * computes the natural logarithm of gamma(x). 
 * if x <= 6.0, then use recursion to get x below 3 
 * then apply rational approximation number 5236 of 
 * hart et al, computer approximations, John Wiley and sons, ny, 1968. 
 * if x > 6.0, then use recursion to get x to at least 12 and 
 * then use formula 5423 of the same source. 
 * 
 * Returns: a double 
 **/

double cdf_alngam_old (double x)
{
  const int c9 = 9;
  const int c4 = 4;
  const int c5 = 5;

  const double hln2pi=0.91893853320467274178E0;
  const double scoefn[9] =
    { 62.003838007127258804, 36.036772530024836321, 20.782472531792126786,
      6.338067999387272343, 2.15994312846059073, .3980671310203570498, 
      .1093115956710439502, .0092381945590275995, .0029737866448101651 };
  const double scoefd[4] =
    { 62.003838007126989331, 9.822521104713994894, -8.906016659497461257,1. };
  const double coef[5] =
    { .083333333333333023564, -.0027777777768818808, 7.9365006754279e-4,
      -5.94997310889e-4, 8.065880899e-4 };

  int i1, i, n;
  double ret_val, d1, d2, prod, xx, offset;

  if (!(x <= 6.))
    {
      goto L70;
    }
  prod = 1.;
  xx = x;
  if (!(x > 3.))
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
  if (!(x < 2.))
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
  d1 = xx - 2.;
  d2 = xx - 2.;
  ret_val =  cdf_devlpl (scoefn, c9, d1) / cdf_devlpl (scoefd, c4, d2);
  /*     COMPUTE RATIONAL APPROXIMATION TO GAMMA(X) */
  ret_val *= prod;
  ret_val = log (ret_val);
  goto L110;
 L70:
  offset = hln2pi;
  /*     IF NECESSARY MAKE X AT LEAST 12 AND CARRY CORRECTION IN OFFSET */
  /*     ADDED TO PREVENT INT OVERFLOW IN int(12.0E0-x) S. STEER */
  if (x > 12.)
    {
      goto L90;
    }

  n = (int) (12. - x);
  if (!(n > 0))
    {
      goto L90;
    }
  prod = 1.;
  i1 = n;
  for (i = 1; i <= i1; ++i)
    {
      prod *= x + (double) (i - 1);
      /* L80: */
    }
  offset -= log (prod);
  xx = x + (double) n;
  goto L100;
 L90:
  xx = x;
  /*     COMPUTE POWER SERIES */
 L100:
  /* Computing 2nd power */
  d2 = xx;
  d1 = 1. / (d2 * d2);
  ret_val = cdf_devlpl (coef, c5, d1) / xx;
  ret_val = ret_val + offset + (xx - .5) * log (xx) - xx;
 L110:
  return ret_val;
} 

/**
 * cdf_alngam:
 * @x: pointer to double 
 * 
 * computes the natural logarithm of gamma(x). 
 * Returns 0 when gamma(x) <0 instead of Nan.
 * 
 * if x <= 6.0, then use recursion to get x in [2,3]  
 * then apply rational approximation number 5236 of 
 * hart et al, computer approximations, John Wiley and sons, ny, 1968. 
 * if x > 6.0, then use recursion to get x in [6,12] 
 * then use formula 5423 of the same source. 
 * 
 * Returns: a double.
 **/

/* C-code reorganized by jpc.
 */ 

double cdf_alngam (double x)
{
  const int c9 = 9;
  const int c4 = 4;
  const int c5 = 5;

  const double hln2pi=0.91893853320467274178E0;
  const double scoefn[9] =
    { 62.003838007127258804, 36.036772530024836321, 20.782472531792126786,
      6.338067999387272343, 2.15994312846059073, .3980671310203570498, 
      .1093115956710439502, .0092381945590275995, .0029737866448101651 };
  const double scoefd[4] =
    { 62.003838007126989331, 9.822521104713994894, -8.906016659497461257,1. };
  const double coef[5] =
    { .083333333333333023564, -.0027777777768818808, 7.9365006754279e-4,
      -5.94997310889e-4, 8.065880899e-4 };

  int i, n;
  double ret_val, d1, d2, prod, xx, offset;

  if ( x > 6 ) 
    {
      offset = hln2pi;
      if (x > 12.)
	{
	  /* bring back x <= 12 */
	  n = (int) (x -12.0);
	  /* now x -n is <= 12 */
	  xx = x - n; 
	  /* gamma(x) = xx*(xx+1)*..*(xx+n-1)*gamma(xx) */
	  prod = xx ;
	  for (i = 1; i < n ; i++) prod *= xx +i; 
	  /* log(gamma(x)) = log(prod) + log(gamma(xx)) */
	  offset += log (prod);
	}
      else 
	{
	  xx = x;
	}
      /* now xx is in the range [6,12]  */
      d2 = xx;
      d1 = 1. / (d2 * d2);
      ret_val = cdf_devlpl (coef, c5, d1) / xx;
      ret_val = ret_val + offset + (xx - .5) * log (xx) - xx;
      return ret_val;
    }
  else 
    {
      /* here x <= 6 */
      prod = 1.;
      xx = x;
      /* bring back x <= 3 */
      while ( xx > 3 ) 
	{
	  xx -= 1;
	  prod *=xx;
	}
      /* now bring x >=2 */
      while ( xx < 2 ) 
	{
	  prod /= xx;
	  xx += 1.;
	}
      /* rational approximation of GAMMA(X) in [0,1] */
      d1 = xx - 2.;
      d2 = xx - 2.;
      ret_val =  cdf_devlpl (scoefn, c9, d1) / cdf_devlpl (scoefd, c4, d2);
      ret_val *= prod;
      /* return 0 when gamma is < 0 */
      return ( prod < 0 ) ? 0 : log(ret_val);
    }
}


