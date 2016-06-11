/* wsqrt.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

  /* 
   *       wsqrt compute the square root of a complex number 
   *       y = yr + i yi = sqrt(x), x = xr + i xi 
   * 
   *       xr,xi: real and imaginary parts of the complex number 
   *       yr,yi: real and imaginary parts of the result 
   *              yr,yi may have the same memory cases than xr et xi 

   *       essentially the classic one which consists in 
   *       choosing the good formula such as avoid cancellation ; 
   *       Also rare spurious overflow are treated with a 
   *       "manual" method. For some more "automated" methods 
   *       (but currently difficult to implement in a portable 
   *       way) see : 
   * 
   *         Hull, Fairgrieve, Tang, 
   *         "Implementing Complex Elementary Functions Using 
   *         Exception Handling", ACM TOMS, Vol. 20 (1994), pp 215-244 
   * 
   *       for xr > 0 : 
   *         yr = sqrt(2( xr + sqrt( xr^2 + xi^2)) )/ 2 
   *         yi = xi / sqrt(2(xr + sqrt(xr^2 + xi^2))) 
   * 
   *       and for xr < 0 : 
   *         yr = |xi| / sqrt( 2( -xr + sqrt( xr^2 + xi^2 )) ) 
   *         yi = sign(xi) sqrt(2(-xr + sqrt( xr^2 + xi^2))) / 2 
   * 
   *       for xr = 0 use 
   *         yr = sqrt(0.5)*sqrt(|xi|)  when |xi| is such that 0.5*|xi| may underflow 
   *            = sqrt(0.5*|xi|)        else 
   *         yi = sign(xi) yr 
   * 
   *       Noting t = sqrt( 2( |xr| + sqrt( xr^2 + yr^2)) ) 
   *                = sqrt( 2( |xr| + pythag(xr,xi) ) ) 
   *       it comes : 
   * 
   *         for xr > 0   |  for xr < 0 
   *        --------------+--------------------- 
   *          yr = 0.5*t  |  yr =  |xi| / t 
   *          yi = xi / t |  yi = sign(xi)*0.5* t 
   * 
   *       as the function pythag must not underflow (and overflow only 
   *       if sqrt(x^2+y^2) > rmax) only spurious (rare) case of overflow 
   *       occurs in which case a scaling is done. 
   * 
   *  Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr> 
   */

static double c_b4 = 1.;
static double c_b6 = .5;
static double c_b8 = 2.;

int nsp_calpack_wsqrt (double *xr, double *xi, double *yr, double *yi)
{
  static int first = TRUE;
  static double rmax;
  double a, b, t;
  static double brmin;
  if (first)
    {
      rmax = C2F (dlamch) ("O", 1L);
      brmin = C2F (dlamch) ("U", 1L) * 2.;
      first = FALSE;
    }
  a = *xr;
  b = *xi;
  if (a == 0.)
    {
      /*       pure imaginary case 
       */
      if (Abs (b) >= brmin)
	{
	  *yr = sqrt (Abs (b) * .5);
	}
      else
	{
	  *yr = sqrt ((Abs (b))) * sqrt (.5);
	}
      *yi = d_sign (&c_b4, &b) * *yr;
    }
  else if (Abs (a) <= rmax && Abs (b) <= rmax)
    {
      /*       standard case : a (not zero) and b are finite 
       */
      t = sqrt ((Abs (a) + nsp_calpack_pythag (&a, &b)) * 2.);
      /*       overflow test 
       */
      if (t > rmax)
	{
	  goto L100;
	}
      /*       classic switch to get the stable formulas 
       */
      if (a >= 0.)
	{
	  *yr = t * .5;
	  *yi = b / t;
	}
      else
	{
	  *yr = Abs (b) / t;
	  *yi = d_sign (&c_b6, &b) * t;
	}
    }
  else
    {
      /*       Here we treat the special cases where a and b are +- 00 or NaN. 
       *       The following is the treatment recommended by the C99 standard 
       *       with the simplification of returning NaN + i NaN if the 
       *       the real part or the imaginary part is NaN (C99 recommends 
       *       something more complicated) 
       */
      if (isnan (a) == 1 || isnan (b) == 1)
	{
	  /*          got NaN + i NaN 
	   */
	  *yr = a + b;
	  *yi = *yr;
	}
      else if (Abs (b) > rmax)
	{
	  /*          case a +- i oo -> result must be +oo +- i oo  for all a (finite or not) 
	   */
	  *yr = Abs (b);
	  *yi = b;
	}
      else if (a < -rmax)
	{
	  /*          here a is -Inf and b is finite 
	   */
	  *yr = 0.;
	  *yi = d_sign (&c_b4, &b) * Abs (a);
	}
      else
	{
	  /*          here a is +Inf and b is finite 
	   */
	  *yr = a;
	  *yi = 0.;
	}
    }
  return 0;
  /*    handle (spurious) overflow by scaling a and b 
   */
L100:
  a /= 16.;
  b /= 16.;
  t = sqrt ((Abs (a) + nsp_calpack_pythag (&a, &b)) * 2.);
  if (a >= 0.)
    {
      *yr = t * 2.;
      *yi = b * 4. / t;
    }
  else
    {
      *yr = Abs (b) * 4. / t;
      *yi = d_sign (&c_b8, &b) * t;
    }
  return 0;
}

