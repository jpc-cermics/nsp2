#include "cdf.h"

/**
 * cdf_dcddexpm1:
 * @x: a double 
 * 
 * computes exp(x)-1  
 * Renaming of function rexp from code of: 
 *   DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant 
 *   Digit Computation of the Incomplete  Beta  Function Ratios.  
 *   ACM  Trans. Math.  Softw. 18 (1993), 360-373.
 * 
 * Returns: a double 
 **/

double cdf_expm1 (double x)
{
  /* test: x= -1:0.01:1; norm(exp(x)-1 - cdf_expm1(x)) */
  const double p1 = 9.14041914819518e-10;
  const double p2 = .0238082361044469;
  const double q1 = -.499999999085958;
  const double q2 = .107141568980644;
  const double q3 = -.0119041179760821;
  const double q4 = 5.95130811860248e-4;
  double w;

  if (Abs (x) <= .15)
    {
      return  x * (((p2 * x + p1) * x + 1.) /
		   ((((q4 * x + q3) * x + q2) * x + q1) * x + 1.));
    }
  w = exp (x);
  if (x > 0.)
    {
      return  exp (x) * (0.5 - 1. / w + 0.5);
    }
  else 
    {
      return  exp (x) - 0.5 - 0.5;
    }
}


