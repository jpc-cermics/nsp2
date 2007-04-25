#include <nsp/machine.h>
#include <nsp/numeric.h>
#include "cdf.h" 

static int largestint();
extern double cdf_dlamch(char *,long int);

/* partial implementation of ipmpar for cdf library 
 * 
 * integers. 
 *     assume integers are represented in the n-digit, base-a form 
 *               sign ( x(n-1)*a**(n-1) + ... + x(1)*a + x(0) ) 
 *               where 0 .le. x(i) .lt. a for i=0,...,n-1. 
 *     ipmpar(1) = a, the base. 
 *     ipmpar(2) = n, the number of base-a digits. 
 *     ipmpar(3) = a**n - 1, the largest magnitude. 
 *  floating-point numbers. 
 *     it is assumed that the single and double precision floating 
 *     point arithmetics have the same base, say b, and that the 
 *     nonzero numbers are represented in the form 
 *               sign (b**e) * (x(1)/b + ... + x(m)/b**m) 
 *               where x(i) = 0,1,...,b-1 for i=1,...,m, 
 *               x(1) .ge. 1, and emin .le. e .le. emax. 
 *     ipmpar(4) = b, the base. 
 *  single-precision 
 *     ipmpar(5) = m, the number of base-b digits. 
 *     ipmpar(6) = emin, the smallest exponent e. 
 *     ipmpar(7) = emax, the largest exponent e. 
 *  double-precision 
 *     ipmpar(8) = m, the number of base-b digits. 
 *     ipmpar(9) = emin, the smallest exponent e. 
 *     ipmpar(10) = emax, the largest exponent e. 
 * 
 *     RWRITTEN BY JPC to use lapack dlamch + a small c program 
 *     for ipmpar(3) 
 */

int cdf_ipmpar(const int *i)
{
  switch (*i) 
    {
    case 3: return largestint();
    case 4: return (int) cdf_dlamch("b", 1L);
    case 9: return (int) cdf_dlamch("m", 1L);
    case 10: return (int) cdf_dlamch("l", 1L);
    default :
      Scierror("ipmpar called with wrong argument\n");
      return 0;
    }
}


/*
 *  MACHINE CONSTANTS
 *  These numbers must be updated when the program is ported to a new machine.
 *  Using spConfig.h to get the largest int 
 */

static int largestint()
{
  static int first=0,large;
  if ( first == 0) 
    {
      if (sizeof(int)==sizeof(long))
	large = LARGEST_LONG_INTEGER ;
      else if (sizeof(int)==sizeof(short))
	large = LARGEST_SHORT_INTEGER;
      else 
	large = 2147483647 ; /** using default value **/
      first++;
      return large ;
    }
  else 
    return large;
}

