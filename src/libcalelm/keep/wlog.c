/* wlog.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int nsp_calpack_wlog (double *xr, double *xi, double *yr, double *yi)
{
  /* Initialized data */

  static int first = TRUE;

  /* System generated locals */
  double d__1;

  /* Local variables */
  static double linf, rmax, lsup;
  double a, b, r__, t;

  /* 
   *    PURPOSE 
   *       wlog compute the logarithm of a complex number 
   *       y = yr + i yi = log(x), x = xr + i xi 
   * 
   *    CALLING LIST / PARAMETERS 
   *       subroutine wlog(xr,xi,yr,yi) 
   *       double precision xr,xi,yr,yi 
   * 
   *       xr,xi: real and imaginary parts of the complex number 
   *       yr,yi: real and imaginary parts of the result 
   *              yr,yi may have the same memory cases than xr et xi 
   * 
   *    METHOD 
   *       adapted with some modifications from Hull, 
   *       Fairgrieve, Tang, "Implementing Complex 
   *       Elementary Functions Using Exception Handling", 
   *       ACM TOMS, Vol. 20 (1994), pp 215-244 
   * 
   *       y = yr + i yi = log(x) 
   *       yr = log(|x|) = various formulae depending where x is ... 
   *       yi = Arg(x) = atan2(xi, xr) 
   * 
   *    AUTHOR 
   *       Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr> 
   * 
   *    PARAMETER 
   *    LOCAL VAR 
   *    CONSTANTS 
   *    EXTERNAL 
   *    STATIC VAR 
   */
  if (first)
    {
      rmax = C2F (dlamch) ("O", 1L);
      linf = sqrt (C2F (dlamch) ("U", 1L));
      lsup = sqrt (rmax * .5);
      first = FALSE;
    }
  /*    (0) avoid memory pb ... 
   */
  a = *xr;
  b = *xi;
  /*    (1) compute the imaginary part 
   */
  *yi = atan2 (b, a);
  /*    (2) compute the real part 
   */
  a = Abs (a);
  b = Abs (b);
  /*    Order a and b such that 0 <= b <= a 
   */
  if (b > a)
    {
      t = b;
      b = a;
      a = t;
    }
  if (.5 <= a && a <= 1.41421356237309504)
    {
      d__1 = (a - 1.) * (a + 1.) + b * b;
      *yr = nsp_calpack_logp1 (&d__1) * .5;
    }
  else if (linf < b && a < lsup)
    {
      /*       no overflow or underflow can occur in computing a*a + b*b 
       */
      *yr = log (a * a + b * b) * .5;
    }
  else if (a > rmax)
    {
      /*       overflow 
       */
      *yr = a;
    }
  else
    {
      t = nsp_calpack_pythag (&a, &b);
      if (t <= rmax)
	{
	  *yr = log (t);
	}
      else
	{
	  /*          handle rare spurious overflow with : 
	   */
	  r__ = b / a;
	  d__1 = r__ * r__;
	  *yr = log (a) + nsp_calpack_logp1 (&d__1) * .5;
	}
    }
  return 0;
}				/* wlog_ */
