  /* 
   *       computes sqrt(a^2 + b^2) with accuracy and 
   *       without spurious underflow / overflow problems 
   * 
   *       This work was motivated by the fact that the original Scilab 
   *       PYTHAG, which implements Moler and Morrison's algorithm is slow. 
   *       Some tests showed that the Kahan's algorithm, is superior in 
   *       precision and moreover faster than the original PYTHAG.  The speed 
   *       gain partly comes from not calling DLAMCH. 
   * 
   *       This is a Fortran-77 translation of an algorithm by William Kahan, 
   *       which appears in his article "Branch cuts for complex elementary 
   *       functions, or much ado about nothing's sign bit", 
   *       Editors: Iserles, A. and Powell, M. J. D. 
   *       in "States of the Art in Numerical Analysis" 
   *       Oxford, Clarendon Press, 1987 
   *       ISBN 0-19-853614-3 
   * 
   *    AUTHOR 
   *       Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr>, 
   *       Thanks to Lydia van Dijk <lvandijk@hammersmith-consulting.com> 
   * 
   *    These constants depend upon the floating point arithmetic of the 
   *    machine.  Here, we give them assuming radix 2 and a 53 bits wide 
   *    mantissa, correspond to IEEE 754 double precision format.  YOU 
   *    MUST RE-COMPUTE THESE CONSTANTS FOR A MACHINE THAT HAS DIFFERENT 
   *    CHARACTERISTIC! 
   * 
   *    (1) r2 must approximate sqrt(2) to machine precision.  The near 
   *        floating point from sqrt(2) is exactly: 
   * 
   *             r2 = (1.0110101000001001111001100110011111110011101111001101)_2 
   *                = (1.4142135623730951454746218587388284504413604736328125)_10 
   *        sqrt(2) = (1.41421356237309504880168872420969807856967187537694807317...)_10 
   * 
   *    (2) r2p1 must approximate 1+sqrt(2) to machine precision. 
   *        The near floating point is exactly: 
   * 
   *         r2p1 = (10.011010100000100111100110011001111111001110111100110)_2 
   *              = (2.41421356237309492343001693370752036571502685546875)_10 
   *    sqrt(2)+1 = (2.41421356237309504880168872420969807856967187537694...)_10 
   * 
   *    (3) t2p1 must approximate 1+sqrt(2)-r2p1 to machine precision, 
   *        this is 
   *                1.25371671790502177712854645019908198073176679... 10^(-16) 
   *        and the near float is exactly: 
   *                (5085679199899093/40564819207303340847894502572032)_10 
   *         t2p1 = (1.253716717905021735741793363204945859....)_10 
   * 
   */

#include "calpack.h"

double nsp_calpack_pythag (double *a, double *b)
{
  static int first = TRUE;
  double ret_val;
  double temp;
  static double rmax;
  double s, t, x, y;

  /* Initialize rmax with computed largest non-overflowing number  */
  
  if (first)
    {
      rmax = nsp_dlamch ("o");
      first = FALSE;
    }

  if (isnan (*a) == 1)
    {
      ret_val = *a;
      return ret_val;
    }

  if (isnan (*b) == 1)
    {
      ret_val = *b;
      return ret_val;
    }
  x = Abs (*a);
  y = Abs (*b);
  /*    Order x and y such that 0 <= y <= x  */
  if (x < y)
    {
      temp = x;
      x = y;
      y = temp;
    }
  /*    Test for overflowing x */
  if (x > rmax)
    {
      ret_val = x;
      return ret_val;
    }
  /*    Handle generic case  */
  t = x - y;
  if (t != x)
    {
      if (t > y)
	{
	  /*  2 < x/y < 2/epsm */
	  s = x / y;
	  s += sqrt (s * s + 1.);
	}
      else
	{
	  /*  1 <= x/y <= 2 */
	  s = t / y;
	  t = (s + 2.) * s;
	  s =
	    t / (sqrt (t + 2.) + 1.41421356237309504) +
	    1.25371671790502177e-16 + s + 2.41421356237309504;
	}
      ret_val = x + y / s;
    }
  else
    {
      ret_val = x;
    }
  return ret_val;
}
