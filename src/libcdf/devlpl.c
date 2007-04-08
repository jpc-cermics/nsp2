#include "cdf.h"

/**
 * cdf_devlpl:
 * @a: Array of coefficients of the polynomial.
 * @n: Length of A, also degree of polynomial - 1.
 * @x: Point at which the polynomial is to be evaluated. 
 * 
 * computes a(1) + a(2)*x + ... + a(n)*x^(n-1) with horner method.
 * 
 * Returns: a double 
 **/

double cdf_devlpl (const double *a,const int n, double x)
{
  double term;
  int i;
  term = a[n -1];
  for (i = n - 2; i >= 0; --i)
    {
      term = a[i] + term * x;
    }
  return  term;
}

